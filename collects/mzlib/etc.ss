
(module etc mzscheme
  (import "spidey.ss")

  (export true false
	  boolean=? symbol=?
	  char->string
	  identity
	  compose

	  build-string
	  build-vector
	  build-list

	  loop-until)

  (define true #t)
  (define false #f)
  
  (define identity (polymorphic (lambda (x) x)))
  
  (define compose
    (polymorphic
     (case-lambda 
      [(f) (if (procedure? f) f (raise-type-error 'compose "procedure" f))]
      [(f g)
       (let ([f (compose f)]
             [g (compose g)])
         (if (eqv? 1 (arity f)) ; optimize: don't use call-w-values
             (if (eqv? 1 (arity g)) ; optimize: single arity everywhere
                 (lambda (x) (f (g x)))
                 (lambda args (f (apply g args))))
             (if (eqv? 1 (arity g)) ; optimize: single input
                 (lambda (a)
                   (call-with-values
                    (lambda () (g a))
                    f))
                 (lambda args
                   (call-with-values
                    (lambda () (apply g args))
                    f)))))]
      [(f . more)
       (let ([m (apply compose more)])
         (compose f m))])))
  

  (define  build-string
    (lambda  (n  fcn)
      (unless  (and (integer? n) (exact? n) (>= n 0))
        (error  'build-string  "~s must be an exact integer >= 0"  n))
      (unless  (procedure? fcn)
        (error  'build-string  "~s must be a procedure"  fcn))
      (let  ((str  (make-string n)))
        (let  loop  ((i  0))
          (if (= i n)  
              str
              (begin
                (string-set!  str  i  (fcn i))
                (loop  (add1 i))))))))
  
  ;; (build-vector n f) returns a vector 0..n-1 where the ith element is (f i).
  ;; The eval order is guaranteed to be: 0, 1, 2, ..., n-1.
  ;; eg: (build-vector 4 (lambda (i) i)) ==> #4(0 1 2 3)
  
  (define  build-vector
    (polymorphic
     (lambda  (n  fcn)
       (unless  (and (integer? n) (exact? n) (>= n 0))
         (error  'build-vector  "~s must be an exact integer >= 0"  n))
       (unless  (procedure? fcn)
         (error  'build-vector  "~s must be a procedure"  fcn))
       (let  ((vec  (make-vector n)))
         (let  loop  ((i  0))
           (if  (= i n)  vec
                (begin
                  (vector-set!  vec  i  (fcn i))
                  (loop  (add1 i)))))))))
  
  (define  build-list
    (polymorphic
     (lambda  (n  fcn)
       (unless  (and (integer? n) (exact? n) (>= n 0))
         (error  'build-list  "~s must be an exact integer >= 0"  n))
       (unless  (procedure? fcn)
         (error  'build-list  "~s must be a procedure"  fcn))
       (if  (zero? n)  '()
            (let  ([head  (list (fcn 0))])
              (let  loop  ([i 1]  [p head])
                (if  (= i n)  head
                     (begin
                       (set-cdr!  p  (list (fcn i)))
                       (loop  (add1 i)  (cdr p))))))))))
  
  (define loop-until
    (polymorphic
     (lambda (start done? next body)
       (let loop ([i start])
         (unless (done? i)
           (body i)
           (loop (next i)))))))
  
  (define boolean=?
    (lambda (x y)
      (unless (and (boolean? x)
                   (boolean? y))
        (raise-type-error 'boolean=? 
                          "boolean"
                          (if (boolean? x) y x)))
      (eq? x y)))
  
  (define (symbol=? x y)
    (unless (and (symbol? x)
		 (symbol? y))
      (raise-type-error 'symbol=? "symbol"
			(if (symbol? x) y x)))
    (eq? x y))

  (define (char->string c)
    (unless (char? c)
      (raise-type-error 'char->string "character" c))
    (string c)))
