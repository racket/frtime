
(module etc mzscheme
  (require "spidey.ss")
  (require-for-syntax (lib "kerncase.ss" "syntax"))

  (provide this-expression-source-directory
           true false
	   boolean=? symbol=?
	   identity
	   compose
	   
	   build-string
	   build-vector
	   build-list
	   
	   loop-until
	   
           opt-lambda
           
	   local
	   recur
	   rec
	   evcase
	   nor
	   nand
	   let+)
  
  (define-syntax (this-expression-source-directory stx)
    (syntax-case stx ()
      [(_)
       (let ([source (syntax-source stx)])
         (if (and source
                  (string? source)
                  (file-exists? source))
             (let-values ([(base file dir?) (split-path source)])
               (with-syntax ([base base])
                 (syntax base)))
             (syntax #f)))]))
    
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

 (define-syntax opt-lambda 
   (lambda (stx)
     (with-syntax ([name (or (syntax-local-name)
			     (quote-syntax opt-lambda-proc))])
       (syntax-case stx ()
	 [(_ args body1 body ...)
	  (let ([clauses (let loop ([pre-args null]
				    [args (syntax args)]
				    [needs-default? #f])
			   (syntax-case args ()
			     [id
			      (identifier? (syntax id))
			      (with-syntax ([(pre-arg ...) pre-args])
				(syntax ([(pre-arg ... . id)
					  body1 body ...])))]
                             [()
			      (with-syntax ([(pre-arg ...) pre-args])
				(syntax ([(pre-arg ...)
					  body1 body ...])))]
			     [(id . rest)
			      (identifier? (syntax id))
			      (begin
				(when needs-default?
				  (raise-syntax-error
				   'opt-lambda
				   "default value missing"
				   stx
				   (syntax id)))
				(loop (append pre-args (list (syntax id)))
				      (syntax rest)
				      #f))]
			     [([id default] . rest)
			      (identifier? (syntax id))
                              (with-syntax ([rest (loop (append pre-args (list (syntax id)))
                                                        (syntax rest)
                                                        #t)]
                                            [(pre-arg ...) pre-args])
                                (syntax ([(pre-arg ...) (name pre-arg ... default)]
                                         . rest)))]
			     [(bad . rest)
			      (raise-syntax-error
			       'opt-lambda
			       "not an identifier or identifier with default"
			       stx
			       (syntax bad))]
			     [else
			      (raise-syntax-error
			       'opt-lambda
			       "bad identifier sequence"
			       stx
			       (syntax args))]))])
            (with-syntax ([clauses clauses])
              (syntax/loc stx
                          (letrec ([name
                                    (case-lambda
                                     . clauses)])
                            name))))]))))
 
 (define-syntax local 
   (lambda (stx)
     (syntax-case stx ()
       [(_ (defn ...) body1 body ...)
	(let ([defs (map
		     (lambda (defn)
		       (let ([d (local-expand
				 defn
				 (kernel-form-identifier-list 
				  (quote-syntax here)))])
			 (syntax-case d (define-values)
			   [(define-values (id ...) body)
			    (for-each
			     (lambda (id)
			       (unless (identifier? id)
				 (raise-syntax-error
				  'local
				  "not an identifier for definition"
				  stx
				  id)))
			     (syntax->list (syntax (id ...))))]
			   [(define-values . rest)
			    (raise-syntax-error
			     'local
			     "ill-formed definition"
			     stx
			     d)]
			   [_else
			    (raise-syntax-error
			     'local
			     "not a definition"
			     stx
			     defn)])
			 d))
		     (syntax->list (syntax (defn ...))))])
	  (let ([ids (apply append
			    (map
			     (lambda (d)
			       (syntax-case d ()
				 [(_ ids . __)
				  (syntax->list (syntax ids))]))
			     defs))])
	    (let ([dup (check-duplicate-identifier ids)])
	      (when dup
		(raise-syntax-error
		 'local
		 "duplicate identifier"
		 stx
		 dup)))
	    (with-syntax ([(def ...) defs])
	      (syntax/loc
	       stx
	       (let ()
		 def ...
		 (let ()
		   body1
		   body ...))))))]
       [(_ x body1 body ...)
	(raise-syntax-error
	 'local
	 "not a definition sequence"
	 stx
	 (syntax x))])))

 ;; recur is another name for 'let' in a named let
 (define-syntax recur 
   (lambda (stx)
     (syntax-case stx ()
       [(_ . rest)
	(syntax/loc stx (let . rest))])))

 ;; define a recursive value
 (define-syntax rec
   (lambda (stx)
     (syntax-case stx ()
       [(_ name expr)
	(begin
	  (unless (identifier? (syntax name))
	    (raise-syntax-error
	     'rec
	     "not an identifier"
	     stx
	     (syntax name)))
	  (syntax/loc stx
	      (letrec ([name expr])
		name)))])))

 (define-syntax evcase 
   (lambda (stx)
     (syntax-case stx ()
       [(_ val [test body ...] ...)
	(with-syntax ([(test ...)
		       (map
			(lambda (t)
			  (syntax-case t (else)
			    [else #t]
			    [_else t]))
			(syntax->list (syntax (test ...))))])
	  (syntax/loc stx
	      (let ([evcase-v val])
		[(eqv? evcase-v test)
		 body ...]
		...)))]
       [(_ val something ...)
	;; Provide a good error message:
	(for-each
	 (lambda (s)
	   (syntax-case s ()
	     [(t a ...)
	      (raise-syntax-error
	       'evcase
	       "invalid clause"
	       stx
	       s)]))
	 (syntax->list (syntax (something ...))))])))
       
 (define-syntax nor
   (lambda (stx)
     (syntax-case stx ()
       [(_ expr ...)
	(syntax/loc stx (not (or expr ...)))])))
 
 (define-syntax nand
   (lambda (stx)
     (syntax-case stx ()
       [(_ expr ...)
	(syntax/loc stx (not (and expr ...)))])))

 (define-syntax let+
   (lambda (stx)
     (syntax-case stx ()
       [(_ [clause ...] body1 body ...)
	(let ([clauses (syntax->list (syntax (clause ...)))]
	      [bad (lambda (c n)
		     (raise-syntax-error
		      'let+
		      (format "illegal use of ~a for a clause" n)
		      stx
		      c))])
	  ;; syntax checks
	  (for-each
	   (lambda (clause)
	     (syntax-case clause (val rec vals recs _)
		  [(val var expr)
		   (identifier? (syntax var))
		   'ok]
		  [(rec var expr)
		   (identifier? (syntax var))
		   'ok]
		  [(vals (var expr) ...)
		   (andmap identifier? (syntax->list (syntax (var ...))))
		   'ok]
		  [(recs (var expr) ...)
		   (andmap identifier? (syntax->list (syntax (var ...))))
		   'ok]
		  [(_ expr)
		   'ok]
		  [(val . _) (bad clause "val")]
		  [(rec . _) (bad clause "rec")]
		  [(vals . _) (bad clause "vals")]
		  [(recs . _) (bad  clause"recs")]
		  [(_ . _) (bad clause "_")]
		  [_else (raise-syntax-error 'let+ "bad clause" stx clause)]))
	   clauses)
	  ;; result
	(let loop ([clauses clauses])
	  (if (null? clauses)
	      (syntax (let () body1 body ...))
	      (with-syntax ([rest (loop (cdr clauses))])
		(syntax-case (car clauses) (val rec vals recs _)
		  [(val var expr)
		   (syntax (let ([var expr]) rest))]
		  [(rec var expr)
		   (syntax (letrec ([var expr]) rest))]
		  [(vals (var expr) ...)
		   (syntax (let ([var expr] ...) rest))]
		  [(recs (var expr) ...)
		   (syntax (letrec ([var expr] ...) rest))]
		  [(_ expr)
		   (syntax (begin expr rest))])))))]))))

