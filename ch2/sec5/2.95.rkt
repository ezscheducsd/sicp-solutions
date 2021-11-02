#lang racket

; Noninteger operations are introduced into the operation,
; which causes difficulties with the gcd algorithm.

(require "2.93-generic-arithmetic.rkt")
(require "2.95-symbolic-algebra.rkt")

(module+ test
  (define var 'x)
  (define p1 (make-polynomial
              var
              '((2 1) (1 -2) (0 1))))
  (define p2 (make-polynomial
              var
              '((2 11) (0 7))))
  (define p3 (make-polynomial
              var
              '((1 13) (0 5))))
  (define q1 (mul p1 p2))
  (define q2 (mul p1 p3))
  p1
  q1
  (define gcd-q1-q2 (greatest-common-divisor q1 q2))
  gcd-q1-q2
  )