
(module struct mzscheme
  (require (lib "etc.ss"))
  
  (provide build-struct-names
	   build-struct-generation
	   build-struct-expand-info
	   struct-declaration-info?)

  ;; build-struct-names : id (list-of id) bool bool -> (list-of id)
  (define build-struct-names
    (opt-lambda (name-stx fields omit-sel? omit-set? [srcloc-stx #f])
      (let ([name (symbol->string (syntax-e name-stx))]
	    [fields (map symbol->string (map syntax-e fields))]
	    [+ string-append])
	(map (lambda (s)
	       (datum->syntax-object name-stx (string->symbol s) srcloc-stx))
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

  (define build-struct-expand-info
    (lambda (name-stx fields omit-sel? omit-set? base-name base-getters base-setters)
      (let* ([names (build-struct-names name-stx fields omit-sel? omit-set?)]
	     [flds (cdddr names)]
	     [every-other (lambda (l)
			    (let loop ([l l])
			      (cond
			       [(null? l) null]
			       [(null? (cdr l)) (list (car l))]
			       [else (cons (car l) (loop (cddr l)))])))]
	     [add-#f (lambda (omit? base)
		       (if omit?
			   (if (let loop ([l base])
				 (cond
				  [(null? l) #t]
				  [(not (car l)) #f]
				  [else (loop (cdr l))]))
			       (append base '(#f)))
			   base))]
	     [qs (lambda (x) (if (eq? x #t)
				 x
				 (and x `(quote-syntax ,x))))])
	`(list-immutable
	  ,(qs (car names))
	  ,(qs (cadr names))
	  ,(qs (caddr names))
	  (list-immutable 
	   ,@(reverse (map qs (every-other flds)))
	   ,@(map qs (add-#f omit-sel? base-getters)))
	  (list-immutable
	   ,@(reverse (map qs (every-other (if (null? flds)
					      null
					      (cdr flds)))))
	   ,@(map qs (add-#f omit-set? base-setters)))
	  ,(qs base-name)))))


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
	 (= (length x) 6)
	 (identifier/#f? (car x))
	 (identifier/#f? (cadr x))
	 (identifier/#f? (caddr x))
	 (id/#f-list? (list-ref x 3))
	 (id/#f-list? (list-ref x 4))
	 (or (eq? #t (list-ref x 5)) (identifier/#f? (list-ref x 5))))))


