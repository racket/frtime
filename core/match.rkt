#lang racket/base

(provide match-fail)

(define-struct a-match-fail ())
(define match-fail (make-a-match-fail))
