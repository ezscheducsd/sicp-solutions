#lang racket

; modify rational arithmetic package to use generic operations
; change make-rat so that it does not reduce to lowest terms.

(require "2.93-generic-arithmetic.rkt")
(require "2.93-symbolic-algebra.rkt")

; We can see that adding two polynomials does not reduce to lowest terms.
; How to reduce to lowest terms?
; use same idea with integers:
; modify make-rat todivide both the numerator and denominator by gcd.

; What is the gcd for polynomials?
; essentially the same procedure as for integers.

(module+ test
  (define p1 (make-polynomial 'x '((1 4) (0 5))))
  (define p2 (make-polynomial 'x '((3 4) (2 -7) (1 -11) (0 5))))
  ; (define rf (make-rational p2 p1))
  ;(add p1 p1)
  ;(add p2 p2)
  (div p2 p1)
  )