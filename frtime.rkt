#lang s-exp "lang-utils.rkt"

(provide value-nowable? behaviorof
         (all-from-out "lang-ext.rkt")
         (all-from-out "lang-utils.rkt")
         (all-from-out "frp-snip.rkt"))

(require "frp-snip.rkt"
         (as-is:unchecked (except-in frtime/core/frp undefined undefined?)
                          event-set? signal-value)
         (except-in "lang-ext.rkt" lift deep-value-now))

(define (value-nowable? x)
  (or (not (signal? x))
      (not (event-set? (signal-value x)))))

(define ((behaviorof pred) x)
  (let ([v (value-now x)])
    (or (undefined? v)
        (pred v))))
