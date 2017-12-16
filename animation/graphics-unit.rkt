#lang racket/base

(provide graphics@)

(require racket/unit
         "graphics-sig.rkt"
         "graphics-posn-less-unit.rkt")

(define-unit posn@ (import) (export graphics:posn^)
  (define-struct posn (x y) #:mutable))

(define-compound-unit/infer graphics@
  (import)
  (export graphics:posn^ graphics:posn-less^) 
  (link posn@ graphics-posn-less@))
