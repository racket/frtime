
(module frtime-tool mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "etc.ss")
           (lib "list.ss")
	   (lib "tool.ss" "drscheme")
           (lib "string-constant.ss" "string-constants"))
  
  (provide tool@)

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (define language-base%
	(class* object% (drscheme:language:simple-module-based-language<%>)
	  (define/public (get-language-numbers)
	    '(1000 -400))
	  (define/public (get-language-position)
	    (list (string-constant experimental-languages) "FrTime"))
	  (define/public (get-module)
	    '(lib "frtime.ss" "frtime"))
	  (define/public (get-one-line-summary)
	    "Language for reactive and time-dependent systems")
          (define/public (get-language-url) #f)
	  (define/public (get-reader)
	    (lambda (name port offsets)
	      (let ([v (read-syntax name port offsets)])
		(if (eof-object? v)
		    v
		    (namespace-syntax-introduce v)))))
	  (super-instantiate ())))

      (define (weak-member obj lis)
        (let ([cmp (lambda (v) (eq? v obj))])
          (let loop ([lis lis])
            (and (cons? lis)
                 (or
                  (cond
                    [(weak-box-value (first lis)) => cmp]
                    [else false])
                  (loop (rest lis)))))))
            
      (define (watch watch-list value as-snip?)
        (foldl
         (lambda (wb acc)
           (cond
             [(weak-box-value wb)
              => (lambda (f) (f acc as-snip?))]
             [else acc]))
         value
         watch-list))
      
      (define language%
	(class (drscheme:language:module-based-language->language-mixin
		(drscheme:language:simple-module-based-language->module-based-language-mixin
		 language-base%))
          (field (watch-list empty))
	  (rename [super-on-execute on-execute])
          (define/override (on-execute settings run-in-user-thread)
            (let ([drs-eventspace (current-eventspace)])
              (super-on-execute settings run-in-user-thread)
              (run-in-user-thread
               (lambda ()
                 (let ([new-watch (namespace-variable-value 'render)]
                       [set-evspc (namespace-variable-value 'set-eventspace)])
                   (set-evspc drs-eventspace)
                   (set! watch-list
                         ((if (weak-member new-watch watch-list)
                              identity
                              (lambda (r) (cons (make-weak-box new-watch) r)))
                          (filter weak-box-value watch-list))))))))

          (rename (super:render-value/format render-value/format)
                  (super:render-value        render-value))
          (override render-value/format render-value)
          (define (render-value/format value settings port put-snip width)
            (super:render-value/format (watch watch-list value put-snip)
                                       settings port put-snip width))
          (define (render-value value settings port put-snip)
            (super:render-value (watch watch-list value put-snip)
                                settings port put-snip))
	  (define/override (use-namespace-require/copy?) #t)
	  (super-instantiate ())))

      (define (phase1) (void))
      (define (phase2)
	(drscheme:language-configuration:add-language 
	 (make-object ((drscheme:language:get-default-mixin) language%)))))))
