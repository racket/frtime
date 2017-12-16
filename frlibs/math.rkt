#lang s-exp frtime/frtime-lang-only
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; math.rkt: some extra math routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide pi e
         (lifted sqr sgn conjugate sinh cosh))

(require (only-in racket/math pi sqr sgn conjugate sinh cosh))
  
;; circular constants and aliases
(define e (exp 1.0))
