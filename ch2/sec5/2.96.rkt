#lang racket

; Implement the procedure pseudoremainder terms,
; which is just like remainder terms except that it multiplies
; the dividend by the integerizing factor before calling div-terms.

; Modify gcd-terms to use pseudoremainder terms, and verify that
; greatest-common-divisor now produces an answer w/ integer coefficients
; as desired in 2.95.

; b. The GCD now has integer coefficients,
; but they are larger than those of P1.
; 

; See 2.95-symbolic-algebra.rkt for the implementation.

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
  (display "q1:")
  (display q1)
  (newline)
  (display "q2:")
  (display q2)
  (newline)
  (display "getting gcd of q1 and q2")
  (newline)
  (greatest-common-divisor q1 q2)
  )