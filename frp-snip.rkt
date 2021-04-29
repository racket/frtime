#lang racket/base
(require racket/match)
(provide (all-defined-out))
(module+ test (require rackunit))

(require framework
         racket/class
         racket/list
         racket/port
         
         ;; FRP requires
         frtime/core/frp
         (except-in frtime/lang-ext
                    undefined?)
         (only-in frtime/lang-core
                  any-nested-reactivity? raise-reactivity)
         
         ;; GRacket require
         racket/gui/base)

(define drs-eventspace #f)

(define (set-eventspace evspc)
  (set! drs-eventspace evspc))

(define value-snip-copy%
  (class string-snip%
    (init-field current parent)
    (inherit get-admin)
    (define/public (set-current c)
      (parameterize ([current-eventspace drs-eventspace])
        (queue-callback
         (lambda ()
           (set! current c)
           (let ([admin (get-admin)])
             (when admin
               (send admin needs-update this 0 0 2000 100)))))))
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (send current draw dc x y left top right bottom dx dy draw-caret))
    (super-instantiate (" "))))

(define (make-snip bhvr)
  (make-object string-snip%
    (let ([tmp (cond
                 [(behavior? bhvr) (value-now bhvr)]
                 [(event? bhvr) (signal-value bhvr)]
                 [else bhvr])])
      (cond
        [(event-set? tmp) (format "#<event (last: ~a@~a)>"
                                  (event-set-events tmp) (event-set-time tmp))]
        [(undefined? tmp) "<undefined>"]
        [else (format "~a" tmp)]))))

(define value-snip%
  (class string-snip%
    (init-field bhvr [ignore-copy-count 1])
    (field [copies empty]
           [current (make-snip bhvr)]
           [loc-bhvr (proc->signal (lambda () (update)) bhvr)])
    
    (define/override (copy)
      (if (> ignore-copy-count 0)
          (begin
            (set! ignore-copy-count (sub1 ignore-copy-count))
            this)
          (let ([ret (make-object value-snip-copy% current this)])
            (set! copies (cons ret copies))
            ret)))
    
    (define/public (update)
      (set! current (make-snip bhvr))
      (for-each (lambda (copy) (send copy set-current current)) copies))
    
    (super-instantiate (" "))))

;; Class of objects to be given to DrRacket for rendering a signal in the
;; interactions window. However, DrRacket won't actually embed this snip
;; directly into the interactions window; instead it makes some number of copies,
;; and one of the copies is what's really inserted into the editor. So
;; we keep references to the copies and propagate changes to them when
;; this copy learns about them.
(define dynamic-snip%
  (class snip%
    (inherit get-admin get-style)
    (define super-new-returned? #f)


    (init-field
     ;; The behavior we want to render dynamically.
     bhvr
     ;; Procedure that generates a rendering of the current value of bhvr.
     super-render-fun)

    ;; width, height : (or/c #f nonnegative-real?)
    ;; when #f, the size information cached inside `current` is wrong
    (define width #f)
    (define height #f)

    (define copies empty)  ; "Copies" of this snip that we need to update.
    (define current (get-rendering (value-now bhvr) super-render-fun))
    (define loc-bhvr (proc->signal (lambda () (update)) bhvr))

    (define/override (copy)
      (define ret (make-object dynamic-snip% bhvr super-render-fun))
      (set! copies (cons ret copies))
      ret)
    
    (define/public (update)
      (define new-current (get-rendering (value-now bhvr) super-render-fun))
      (parameterize ([current-eventspace drs-eventspace])
        (queue-callback
         (λ ()
           (update-current new-current)))))

    (define/public (update-current new-current)
      (set! current new-current)
      (request-refresh)
      (for-each (lambda (copy) (send copy update-current current)) copies))

    (define/private (request-refresh)
      (set! width #f)
      (set! height #f)
      (when super-new-returned?
        (define admin (get-admin))
        (when admin
          (send admin resized this #t))))
    
    (define/override (size-cache-invalid)
      (set! width #f)
      (set! height #f)
      (for-each
       (lambda (s) (send s size-cache-invalid))
       copies))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (unless (and width height) (recalculate-size-information dc))
      (for ([line (in-list current)])
        (match-define (vector y-baseline-offset info+words) line)
        (for ([info+word (in-list info+words)])
          (match-define (vector x-offset height-above-baseline word) info+word)
          (define draw-x (+ x x-offset))
          (define draw-y (+ y y-baseline-offset (- height-above-baseline)))
          (cond
            [(string? word)
             (send dc draw-text word draw-x draw-y)]
            [else
             (send word draw dc draw-x draw-y
                   left top right bottom
                   0 0  ;; these are wrong but maybe not needed by most snips
                   #f)]))))

    (define/override (get-extent dc x y wb hb descent space lspace rspace)
      (unless (and width height) (recalculate-size-information dc))
      (define (set-box/f b v) (when (box? b) (set-box! b v)))
      (set-box/f wb width)
      (set-box/f hb height)
      (set-box/f descent 0)
      (set-box/f space 0)
      (set-box/f lspace 0)
      (set-box/f rspace 0))

    (define/private (recalculate-size-information dc)
      (define font (send (get-style) get-font))
      (define wb (box 0))
      (define hb (box 0))
      (define db (box 0))
      (define-values (total-width total-height)
        (for/fold ([total-width 0]
                   [total-height 0])
                  ([line (in-list current)])
          (define-values (x-offset line-height-above-baseline line-descent)
            (for/fold ([x-offset 0]
                       [line-height-above-baseline 0]
                       [line-descent 0])
                      ([x-off+hab+word (in-list (vector-ref line 1))])
              (define word (vector-ref x-off+hab+word 2))
              (define-values (this-w this-h this-d)
                (cond
                  [(string? word)
                   (define-values (lw lh ld _) (send dc get-text-extent word font))
                   (values lw lh ld)]
                  [else
                   (send word set-style (get-style))
                   ;; we pass 0 as x and y here which means that a snip whose size depends
                   ;; on its position will always have the size it would have at (0,0)
                   (send word get-extent dc 0 0 wb hb db #f #f #f)
                   (values (unbox wb) (unbox hb) (unbox db))]))
              (define this-height-above-baseline (- this-h this-d))
              (vector-set! x-off+hab+word 0 x-offset)
              (vector-set! x-off+hab+word 1 this-height-above-baseline)
              (values (+ x-offset this-w)
                      (max this-height-above-baseline line-height-above-baseline)
                      (max this-d line-descent))))
          (vector-set! line 0 (+ total-height line-height-above-baseline))
          (values (max x-offset total-width)
                  (+ total-height line-height-above-baseline line-descent))))
      (set! width total-width)
      (set! height total-height))

    (super-new)
    (set! super-new-returned? #t)))

(define (render beh as-snip?)
  (cond
    [as-snip? (watch beh)]
    [(undefined? (value-now beh)) "<undefined>"]
    [(behavior? beh) (format "#<behavior (~a)>" (value-now beh))]
    [(event? beh) (format "#<event (last: ~a)>" (event-set-events (signal-value beh)))]
    [else beh]))

(define (render/dynamic-snip val super-render-fun)
  (if (behavior? val)
      ; interesting case:
      ; create a snip
      ; each time val changes, recompute its rendering via super-render-fun
      (make-object dynamic-snip% val super-render-fun)
      ; easy case
      (super-render-fun val)))

;; get-rendering : any (any port -> void) ->
;; (listof (vector y-baseline-offset (listof (vector x-offset height-above-baseline (string U snip%)))))
;; Applies super-render-fun to val and a port. Returns information about
;; what to print; the outer list are lines and the inner lists are the content
;; of the lines in order; the vectors to record spacing information (which is computed elsewhere)
(define (get-rendering val super-render-fun)
  (define-values (in out) (make-pipe-with-specials))
  (thread (λ () (super-render-fun val out) (close-output-port out)))
  (let loop ([lines '()])
    (define line (read-a-line in))
    (if (eof-object? line)
        (map (λ (x) (vector 0 x)) (reverse lines))
        (loop (cons (map (λ (x) (vector 0 0 x)) line) lines)))))

(define (read-a-line in)
  (define pending-word '())
  (define pending-line '())
  (define (finish-word)
    (unless (null? pending-word)
      (set! pending-line (cons (apply string (reverse pending-word))
                               pending-line))
      (set! pending-word '())))
  (let loop ()
    (define c (read-char-or-special in))
    (cond
      [(eof-object? c)
       (finish-word)
       (cond
         [(null? pending-line) c]
         [else (reverse pending-line)])]
      [(equal? c #\newline)
       (finish-word)
       (reverse pending-line)]
      [(char? c)
       (set! pending-word (cons c pending-word))
       (loop)]
      [(is-a? c snip%)
       (finish-word)
       (set! pending-line (cons c pending-line))
       (loop)]
      [else
       (finish-word)
       (set! pending-line (cons (format "~s" c) pending-line))
       (loop)])))

(module+ test
  (check-equal? (read-a-line (open-input-string "")) eof)
  (check-equal? (read-a-line (open-input-string "a")) '("a"))
  (check-equal? (read-a-line (open-input-string "ab")) '("ab"))
  (check-equal? (read-a-line (open-input-string "ab\nc")) '("ab"))
  (let ()
    (define-values (in out) (make-pipe-with-specials))
    (define s (new snip%))
    (write-special s out)
    (close-output-port out)
    (check-equal? (read-a-line in) (list s)))
  (let ()
    (define-values (in out) (make-pipe-with-specials))
    (define s (new snip%))
    (display "ab" out)
    (write-special s out)
    (display "cd" out)
    (close-output-port out)
    (check-equal? (read-a-line in) (list "ab" s "cd")))
  (let ()
    (define-values (in out) (make-pipe-with-specials))
    (define s (new snip%))
    (display "ab" out)
    (write-special s out)
    (display "cd\nef" out)
    (close-output-port out)
    (check-equal? (read-a-line in) (list "ab" s "cd"))))

(define (watch beh super-render-fun)
  (cond
    [(undefined? beh)
     (begin
       (make-object string-snip% "<undefined>")
       )
     ]
    [(event? beh)
     (make-object value-snip% beh)]
    [(or (behavior? beh) (any-nested-reactivity? beh))
     (make-object dynamic-snip% (raise-reactivity beh) super-render-fun)]
    [(signal? beh)
     (make-object dynamic-snip% beh super-render-fun)]
    [else beh]))
