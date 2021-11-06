#lang racket

(require sicp-helpers/mathlib)

(provide compose)

; (compose f g)
; is a function that takes a single input x
; and returns f(g(x))
(define (compose f g)
  (lambda (x) (f (g x))))

(module+ test
  (require rackunit)
  (check-eq? ((compose square inc) 6) 49))