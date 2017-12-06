#lang s-exp frtime/frtime-lang-only
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; math.rkt: some extra math routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (only-in racket/math pi sqr sgn conjugate sinh cosh))
  
(provide (lifted sqr sgn conjugate sinh cosh))
  
(provide pi e)
  
;; circular constants and aliases
(define e (exp 1.0))
