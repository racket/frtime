(module list (lib "frtime.ss" "frtime")
  
  (require (lift/strict (lib "list.ss")
                        first second third fourth fifth sixth seventh eighth
                        last-pair remq remq* remv remv* rest empty? cons?)
           (as-is (lib "list.ss") empty))
  
  (define (assf f l)
    (if (and (cons? l) (f (first (first l))))
        (first l)
        #f))
  
#|  (define foldl
    (case-lambda
      [(f init l) ])) |#
  
  (define (filter f l)
    (cond
      [(empty? l) empty]
      [(f (first l)) (cons (first l) (filter f (rest l)))]
      [else (filter f (rest l))]))
  
  (provide (all-defined-except) empty))