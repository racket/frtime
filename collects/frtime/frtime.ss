(module frtime (lib "frp.ss" "frtime")
  
  (require (all-except mzscheme
                       module
                       #%app
                       #%top
                       #%datum
                       #%plain-module-begin
                       #%module-begin
                       lambda
                       case-lambda
                       define-values
                       define
                       let
                       letrec
                       let-values
                       let*
                       begin
                       begin0
                       quote
                       quasiquote
                       unquote
                       values
                       if
                       require
                       provide
                       and
                       or
                       cond
                       map ormap andmap assoc member)
           (lib "list.ss"))
  
  (define-syntax cond
    (syntax-rules (else =>)
      [(_ [else result1 result2 ...])
       (begin result1 result2 ...)]
      [(_ [test => result])
       (let ([temp test])
         (if temp (result temp)))]
      [(_ [test => result] clause1 clause2 ...)
       (let ([temp test])
         (if temp
                 (result temp)
                 (cond clause1 clause2 ...)))]
      [(_ [test]) test]
      [(_ [test] clause1 clause2 ...)
       (let ((temp test))
         (if temp
                 temp
                 (cond clause1 clause2 ...)))]
      [(_ [test result1 result2 ...])
       (if test (begin result1 result2 ...))]
      [(_ [test result1 result2 ...]
          clause1 clause2 ...)
       (if test
               (begin result1 result2 ...)
               (cond clause1 clause2 ...))]))
  
  (define-syntax and
    (syntax-rules ()
      [(_) #t]
      [(_ exp) exp]
      [(_ exp exps ...) (if exp
                            (and exps ...)
                            #f)]))
  
  (define-syntax or
    (syntax-rules ()
      [(_) #f]
      [(_ exp) exp]
      [(_ exp exps ...) (let ([v exp])
                          (if v
                              v
                              (or exps ...)))]))
  
  (define (ormap proc lst)
    (and (lift #t cons? lst)
         (or (proc (lift #t first lst)) (ormap proc (lift #t rest lst)))))
  
  (define (andmap proc lst)
    (or (lift #f empty? lst)
        (and (proc (lift #t first lst)) (andmap proc (lift #t rest lst)))))
  
  (define-syntax frtime:case
    (syntax-rules ()
      [(_ exp clause ...)
       (let ([v exp])
         (vcase v clause ...))]))
  
  (define-syntax vcase
    (syntax-rules (else)
      [(_ v [else exp ...])
       (begin exp ...)]
      [(_ v [dl exp ...])
       (if (lift #t memv v (quote dl))
           (begin exp ...))]
      [(_ v [dl exp ...] clause ...)
       (if (lift #t memv v (quote dl))
           (begin exp ...)
           (vcase v clause ...))]))
  
  (define map
    (let ([first (lambda (l) (lift #t first l))]
          [rest (lambda (l) (lift #t rest l))]
          [cons (lambda (f r) (lift #f cons f r))]
          [cons? (lambda (v) (lift #t cons? v))])
      (case-lambda
        [(f l) (if (cons? l)
                   (cons (f (first l)) (map f (rest l)))
                   empty)]
        [(f l . ls) (if (and (cons? l) (andmap cons? ls))
                        (cons (lift #f apply f (first l) (map first ls)) (lift #f apply map f (rest l) (map rest ls)))
                        empty)])))

  ; TO DO: assoc member [vectors] structs

  ; first cut: could be made more efficient by creating
  ; a dedicated signal to update each element of the vector
  (define (frtime:vector . args)
    (let* ([n (length args)]
           [v1 (make-vector n)]
           [v2 (make-vector n)])
      (apply
       proc->signal
       (lambda ()
         (let ([tmp v2])
           (set! v2 v1)
           (set! v1 tmp))
         (let loop ([i 0] [args args])
           (when (< i n)
             (vector-set! v1 i (get-value (first args)))
             (loop (add1 i) (rest args))))
         v1)
       args)))
  
  (provide (lifted + - * / = eq? equal? eqv? < > <= >= list? add1 cos sin tan symbol->string symbol?
                   number->string exp expt even? odd? list-ref string-append pair?
                   sub1 sqrt not number? string? zero? min max modulo car cdr null?
                   string->number format void? rational? char? char-upcase char-ci>=? char-ci<=?
                   string>=? char-locale-upcase char-upper-case? char-alphabetic? char-locale-ci>?
                   char-locale-ci<? string<? char-locale-ci=? string-ci=? string-locale-ci>?
                   string-locale-ci<? string-locale-ci=? atan asin acos exact? magnitude imag-part
                   real-part numerator abs log lcm gcd arithmetic-shift integer-sqrt make-rectangular
                   integer-byte-string->integer integer->integer-byte-string complex? char>? char<? char=?
                   char-numeric? date-time-zone-offset list->string substring string->list
                   string-ci<? string-ci>=? string<=? string-ci<=? string>? string-locale<? string=?
                   string-length string-ref char-locale-downcase char-locale-lower-case? char-locale-upper-case?
                   char-locale-whitespace? char-locale-numeric? char-locale-alphabetic? floor angle round
                   ceiling real? date-hour vector-ref procedure?
                   rationalize date-year-day date-week-day date? date-dst? date-year date-month date-day
                   date-minute date-second make-date char-downcase char>=? char<=? char->integer boolean?
                   integer? quotient remainder positive? negative? inexact->exact exact->inexact
                   make-polar denominator truncate bitwise-not bitwise-xor bitwise-and bitwise-ior inexact?
                   char-whitespace? assq assv memq memv list-tail reverse append length seconds->date)
           (rename frtime:case case)
           (rename frtime:vector vector)
           (rename eq? mzscheme:eq?)
           (lifted/nonstrict cons list apply)
           null gensym collect-garbage
           error define-struct set! printf for-each void when unless
           procedure-arity-includes? raise-type-error raise
           make-exn:application:mismatch current-continuation-marks
           raise-mismatch-error
           cond and or andmap ormap map
           (all-from-except (lib "frp.ss" "frtime"))))
