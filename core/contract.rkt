#lang racket/base

(require racket/provide-syntax
         #;(for-syntax racket/contract))

(define-provide-syntax contract-out*
  (syntax-rules ()
    [(contract-out* [id ctrct] ...)
     #;(contract-out [id ctrct] ...)
     (combine-out id ...)]))

(provide contract-out*)
