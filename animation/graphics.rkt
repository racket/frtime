#lang racket/base

(require racket/unit "graphics-sig.rkt")
(provide-signature-elements graphics:posn^
                            graphics:posn-less^)

(require racket/gui/base
         "graphics-unit.rkt")

(define-values/invoke-unit/infer graphics@)
