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
                       map ormap andmap)
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
  
  (provide (lift/strict + - * / = equal? < > <= >= list? add1 cos sin tan symbol->string symbol?
                        number->string exp expt even? odd? list-ref string-append
                        sub1 sqrt not apply number? string? zero? min max modulo car cdr null?
                        string->number format)
           (lift cons list)
           eq?
           error define-struct set! printf for-each void when
           cond and or andmap ormap map
           (all-from-except (lib "frp.ss" "frtime"))))
