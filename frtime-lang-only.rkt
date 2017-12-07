#lang s-exp "lang-utils.rkt"

(provide behaviorof
         value-nowable?
         (all-from-out "lang-utils.rkt")
         (all-from-out frtime/lang-ext))

(require (as-is:unchecked (except-in frtime/core/frp undefined? undefined) 
                          event-set? signal-value)
         (only-in frtime/lang-ext undefined? signal? value-now lift))
  
(define (value-nowable? x)
  (or (not (signal? x))
      (not (event-set? (signal-value x)))))

(define ((behaviorof pred) x)
  (let ([v (value-now x)])
    (or (undefined? v)
        (pred v))))
