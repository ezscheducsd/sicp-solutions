#lang racket

; Using div-terms, implement the procedure
; remainder terms and use this to define gcd-terms above.
; then write procedure gcd-poly

; We kinda already did this in 2.94

(require "2.93-symbolic-algebra.rkt")
(require "2.93-generic-arithmetic.rkt")

(module+ test
  (define p1 (make-polynomial
              'x '((4 1) (3 -1) (2 -2) (1 2))))
  (define p2 (make-polynomial 'x '((3 1) (1 -1))))
  (greatest-common-divisor p1 p2)
  )