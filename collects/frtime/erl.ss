(module erl mzscheme
  
  (require (lib "list.ss")
           (lib "thread.ss")
           (lib "etc.ss")
           (lib "dns.ss" "net")
           "mymatch.ss")
    
  (define ip-address
    (let-values ([(sub-proc in-p dummy1 dummy2) (subprocess #f #f #f "/bin/hostname" "-i")])
      (begin0
        (read in-p)
        (subprocess-wait sub-proc))))

  (define dns
    (dns-find-nameserver))
  
  (define ip-regexp
    (regexp "[0-9][0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?"))
  
  ; a tid is a (vector 'tid symbol(ip) num(port) symbol(gensym))
  
  (define make-tid
    (case-lambda
      [(thr) (vector 'tid ip-address thr)]
      [(host thr) (vector 'tid (if (regexp-match ip-regexp (symbol->string host))
                                   host
                                   (string->symbol (dns-get-address dns (symbol->string host))))
                          thr)]))
  
  (define (tid-ip tid)
    (vector-ref tid 1))
  
  (define (tid-lid tid)
    (vector-ref tid 2))
  
  ; We need a mapping from MzScheme's tids to our tids (just for `self')
  ; and a mapping from symbols to mailboxes (for local threads).
  ; A special thread is responsible for all communication with external threads.
  ; All processes spawned on a node have same ip-address.
  
  (define tids
    (make-hash-table 'weak))
  
  (define mailboxes
    (make-hash-table))
    
  (define-struct mailbox (old-head old-last head tail sem-count lock-enqueue))
  
  (define (try-extract m l)
    (let loop ([prev l] [cur (rest l)])
      (if (empty? (rest cur))
          match-fail
          (let ([v (m (first cur))])
            (if (eq? v match-fail)
                (loop cur (rest cur))
                (begin
                  (set-rest! prev (rest cur))
                  v))))))
  
  (define (receive-help timeout timeout-thunk matcher)
    (if (and timeout (negative? timeout))
        (timeout-thunk)
        (let* ([start-time (current-milliseconds)]
               [mb (hash-table-get mailboxes (tid-lid (self)))]
               [val (try-extract matcher (mailbox-old-head mb))])
          (if (eq? val match-fail)
              (let loop ()
                (let* ([elapsed (- (current-milliseconds) start-time)]
                       [wait-time (cond
                                    [(not timeout) false]
                                    [(> elapsed timeout) 0]
                                    [else (/ (- timeout elapsed) 1000)])]
                       [val (object-wait-multiple wait-time (mailbox-sem-count mb))])
                  (if val
                      (let* ([oldhead (mailbox-head mb)]
                             [msg (first oldhead)]
                             [val (begin
                                    (set-mailbox-head! mb (rest oldhead))
                                    (matcher msg))])
                        (if (eq? val match-fail)
                            (let ([new-last (cons empty empty)]
                                  [old-last (mailbox-old-last mb)])
                              (set-first! old-last msg)
                              (set-rest! old-last new-last)
                              (set-mailbox-old-last! mb new-last)
                              (loop))
                            (val)))
                      (timeout-thunk))))
              (val)))))
  
  (define-syntax receive
    (syntax-rules (after)
      [(_ (after timeout to-expr ...) (pat expr ...) ...)
       (let* ([matcher (match-lambda (pat (lambda () expr ...)) ...)]
              [timeout-thunk (lambda () to-expr ...)])
         (receive-help timeout timeout-thunk matcher))]
      [(_ clause ...) (receive (after false (void)) clause ...)]))
  
  ; must ensure name not already taken
  (define (spawn/name-help thunk name)
    (if (hash-table-get mailboxes name (lambda () #f))
        #f
        (let ([new-tid (vector 'tid ip-address name)]
              [parent-tid (self)])
          (thread
           (lambda ()
             (hash-table-put! tids (current-thread) new-tid)
             (hash-table-put! mailboxes name (new-mailbox))
             (! parent-tid new-tid)
             (thunk)))
          (receive [(? (lambda (m) (equal? m new-tid))) new-tid]))))
  
  (define last-thread 1)
  
  (define next-thread
    (let ([lock (make-semaphore 1)])
      (lambda ()
        (with-semaphore
         lock
         (lambda ()
           (begin0
             last-thread
             (set! last-thread (add1 last-thread))))))))
  
  (define-syntax spawn
    (syntax-rules ()
      [(_ expr ...) (spawn/name-help (lambda () expr ...)
                                     (string->symbol
                                      (string-append "thread" (number->string (next-thread)))))]))
  
  (define-syntax spawn/name
    (syntax-rules ()
      [(_ name expr ...) (spawn/name-help (lambda () expr ...) name)]))
  
  (define (new-mailbox)
    (let* ([sentinel (cons empty empty)]
           [old-sentinel (cons empty empty)]
           [old-head (cons empty old-sentinel)])
      (make-mailbox old-head
                    old-sentinel
                    sentinel
                    sentinel
                    (make-semaphore)
                    (make-semaphore 1))))
  
  (define main (vector 'tid ip-address (string->symbol "main")))
  (hash-table-put! tids (current-thread) main)
  (hash-table-put! mailboxes (tid-lid main) (new-mailbox))
  
  (define forward-mailbox (new-mailbox))
  
  (define network-up? #f)
  
  ; forwarder
  (define (start-network)
    (if network-up?
        #t
        (with-handlers ([(lambda (exn) true) (lambda (exn)
                                               (printf "failed to start network: ~a~n" exn) false)])
          (let ([listener (tcp-listen 20000 4 #t #f)])
            (thread
             (lambda ()
               (let* ([in-ports (make-hash-table)]
                      [out-ports (make-hash-table)]
                      [mk-wait-set (lambda () (apply waitables->waitable-set
                                                     (hash-table-map in-ports (lambda (key val) val))))])
             (let loop ([wait-set (mk-wait-set)])
               (void "have connections to ~a~n" (hash-table-map in-ports (lambda (k v) k)))
               (let ([val (object-wait-multiple #f (mailbox-sem-count forward-mailbox)
                                                listener wait-set)])
                 (cond
                   [(tcp-listener? val)
                    (let*-values ([(in-p out-p) (tcp-accept listener)]
                                  [(local-ip remote-ip) (tcp-addresses out-p)])
                      ; clean up eventually to handle near-simultaneous mutual connections
                      ; (idea: use natural ordering on IP addresses to choose which connection survives)
                      ; (also: need to close and remove dead ports)
                      (hash-table-put! in-ports (string->symbol remote-ip) in-p)
                      (hash-table-put! out-ports (string->symbol remote-ip) out-p)
                      (loop (mk-wait-set)))]
                   [(input-port? val)
                    (with-handlers ([(lambda (exn) (or (exn:i/o:port? exn) (exn:read? exn)))
                                     (lambda (exn) (printf "~a~n" exn) (loop wait-set))])
                      (match (read val)
                        [(lid msg)
                         (let ([mb (hash-table-get mailboxes lid (lambda () false))])
                           (when mb (send-msg mb msg))
                           (loop wait-set))]
                        [(? eof-object?)
                         (let*-values ([(local-ip-str remote-ip-str) (tcp-addresses val)]
                                       [(remote-ip) (string->symbol remote-ip-str)]
                                       [(out-p) (hash-table-get out-ports remote-ip)])
                           (close-input-port val)
                           (close-output-port out-p)
                           (hash-table-remove! in-ports remote-ip)
                           (hash-table-remove! out-ports remote-ip)
                           (loop (mk-wait-set)))]))] ; probably want to close port, remove from hash table
                   [else ; val was the mailbox semaphore
                    (match (first (mailbox-head forward-mailbox))
                      ['quit (void)]
                      [(#('tid ip lid) msg)
                       (let ([out-p (hash-table-get
                                     out-ports ip
                                     (lambda ()
                                       (let-values ([(in-p out-p)
                                                     (with-handlers ([exn:i/o:tcp? (lambda (dummy) (values #f #f))])
                                                       (tcp-connect (symbol->string ip) 20000))])
                                         (when out-p
                                           (hash-table-put! in-ports ip in-p)
                                           (hash-table-put! out-ports ip out-p)
                                           (set! wait-set (mk-wait-set))
                                           out-p))))])
                         (when out-p
                           (with-handlers ([exn:i/o:port? (lambda (exn) (printf "~a~n" exn))])
                             (write (list lid msg) out-p)))
                         (set-mailbox-head! forward-mailbox (rest (mailbox-head forward-mailbox)))
                         (loop wait-set))])]))))))
            (set! network-up? #t)
            #t))))
    
  (define (stop-network)
    (when network-up?
      (send-msg forward-mailbox 'quit)
      (set! network-up? #f)))
  
  (define (local? tid)
    (eq? (tid-ip tid) ip-address))
  
  (define (! tid msg)
    (if (local? tid)
        (let ([mb (hash-table-get mailboxes (tid-lid tid) (lambda () false))])
          (when mb
            (send-msg mb msg)))
        (when network-up?
          (send-msg forward-mailbox (list tid msg))))) ; forward via special thread
  
  (define (send-msg mbox msg)
    (with-semaphore
     (mailbox-lock-enqueue mbox)
     (lambda ()
        (let ([newtail (cons empty empty)]
              [oldtail (mailbox-tail mbox)])
          (set-first! oldtail msg)
          (set-rest! oldtail newtail)
          (set-mailbox-tail! mbox newtail)
          (semaphore-post (mailbox-sem-count mbox))))))

  (define (self)
    (hash-table-get tids (current-thread)
                    (lambda ()
                      (let* ([name (string->symbol
                                    (string-append "thread" (number->string (next-thread))))]
                             [new-tid (vector 'tid ip-address name)])
                        (hash-table-put! tids (current-thread) new-tid)
                        (hash-table-put! mailboxes name (new-mailbox))
                        new-tid))))
  
  (define (!! msg)
    (let ([mb (hash-table-get mailboxes (tid-lid (self)) (lambda () false))])
      (if mb
          (let ([new-last (cons empty empty)]
                [old-last (mailbox-old-last mb)])
            (set-first! old-last msg)
            (set-rest! old-last new-last)
            (set-mailbox-old-last! mb new-last)))))
  
  (define (mybox)
    (hash-table-get mailboxes (self)))
  
  (provide
;   mailboxes
;   mybox
;   (struct mailbox (old-head old-last channel))
   make-tid
   spawn
   spawn/name
   start-network
   stop-network
   network-up?
   !
   !!
   receive
   self))

#|
(require erl)

(define (send-loop n)
  (let ([me (self)])
    (let loop ([i 0])
      (if (>= i n)
          void
          (begin
            (! me true)
            (loop (+ i 1)))))))

(define (send-loop2 n)
  (let loop ([i 0])
    (if (>= i n)
        void
        (begin
          (!! true)
          (loop (+ i 1))))))

(define (flush-queue)
  (let recur ()
    (receive [after 0 void]
             [_ (recur)])))

(define (mybox) (hash-table-get mailboxes (self)))
|#