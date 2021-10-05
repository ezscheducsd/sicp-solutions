#lang racket

; generic equality predicate equ?
; tests the equality of two numbers.
; install it in the generic arithmetic package.
; should work for all types.

(require "../sec4/2.4.3-complex-data-directed.rkt")
(require "2.5.1-generic-arithmetic.rkt")

(module+ test
  (require rackunit)
  (define r1 (make-rational 3 -6))
  (define r2 (make-rational -5 10))
  (define r5 (make-rational 1 2))
  (check-true (equ? r1 r2))
  (define r3 (make-rational 0 4))
  (define r4 (make-rational 0 -5))
  (check-true (equ? r3 r4))
  (check-false (equ? r2 r5))

  (check-false (equ? 3 4))
  (check-true (equ? -1 -1))

  (define c1 (make-complex-from-real-imag -1 0))
  (define c2 (make-complex-from-mag-ang 1 pi))
  (define c3 (make-complex-from-mag-ang 1 (/ (* 3 pi) 2)))
  (check-true (equ? c1 c2))
  (check-false (equ? c2 c3))
  )