
(module struct mzscheme
  
  (provide build-struct-names
	   build-struct-generation
	   struct-declaration-info?)

  ;; build-struct-names : id (list-of id) bool bool -> (list-of id)
  (define build-struct-names
    (lambda (name-stx fields omit-sel? omit-set?)
      (let ([name (symbol->string (syntax-e name-stx))]
	    [fields (map symbol->string (map syntax-e fields))]
	    [+ string-append])
	(map (lambda (s)
	       (datum->syntax-object name-stx (string->symbol s) #f))
	     (append
	      (list 
	       (+ "struct:" name)
	       (+ "make-" name)
	       (+ name "?"))
	      (let loop ([l fields])
		(if (null? l)
		    null
		    (append
		     (if omit-sel?
			 null
			 (list (+ name "-" (car l))))
		     (if omit-set?
			 null
			 (list (+ "set-" name "-" (car l) "!")))
		     (loop (cdr l))))))))))

  (define build-struct-generation
    (lambda (name-stx fields omit-sel? omit-set?)
      (let ([names (build-struct-names name-stx fields omit-sel? omit-set?)])
	(let ([name name-stx]
	      [num-fields (length fields)]
	      [acc/mut-makers (let loop ([l fields][n 0])
				(if (null? l)
				    null
				    (let ([mk-one
					   (lambda (acc?)
					     (list
					      `(,(if acc?
						     'make-struct-field-accessor
						     'make-struct-field-mutator)
						,(if acc? 'acc 'mut)
						,n ',(car l))))])
				      (append
				       (if omit-sel?
					   null
					   (mk-one #t))
				       (if omit-set?
					   null
					   (mk-one #f))
				       (loop (cdr l) (add1 n))))))])
	  `(let-values ([(struct: make- ? acc mut)
			 (make-struct-type ',name #f ,num-fields 0 #f null #f)])
	     (values struct:
		     make-
		     ?
		     ,@acc/mut-makers))))))

  (define (struct-declaration-info? x)
    (define (identifier/#f? x)
      (or (not x) 
	  (identifier? x)))
    (define (id/#f-list? x)
      (or (null? x)
	  (and (pair? x)
	       (if (null? (cdr x))
		   (identifier/#f? (car x))
		   (and (identifier? (car x))
			(id/#f-list? (cdr x)))))))

    (and (list? x)
	 (= (length x) 5)
	 (identifier/#f? (car x))
	 (identifier/#f? (cadr x))
	 (identifier/#f? (caddr x))
	 (id/#f-list? (list-ref x 3))
	 (id/#f-list? (list-ref x 4)))))


