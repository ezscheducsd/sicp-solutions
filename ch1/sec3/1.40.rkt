#lang racket

(require "1.3.4-newtons-method.rkt")
(require sicp-helpers/mathlib)

; (cubic a b c) - the function that, given x,
; return x^3 + ax^2 + bx + c

(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

; root of x^3 + x^2 + 2x + 3 should be about -1.276
(module+ test
  (newtons-method (cubic 1 2 3) 1))