#lang racket

; univariate polynomials can be divided by other
; univariate polynomials to produce a polynom. quotient
; and a polynomial remainder.

; Division can be performed via long division.
; Divide highest order term of dividend
; by highest-order term of divisor.

; Result is the first term of the quotient.
; then multiply the result by the divisor, subtract that from the dividend,
; then produce the rest of the answer by dividing the difference
; by the divisor.
; Stop when the order of the divisor exceeds the order of the dividend,
; and declare the dividend to be the remainder.
; If the dividend ever becomes zero, return zero as both quotient and remainder.

; Example: x^5 - 1 / x^2 - 1
; x^5/x^2 = x^3 (first term of quotient)
; Temp-result: x^3 (x^2 - 1) = (x^5 - x^3)
; Subtract from dividend: (x^5 - 1) - (x^5 - x^3) = -x^3 - 1
; Then divide this difference by the divisor.

(require "2.5.2-generic-arithmetic-raise.rkt")
(require "2.5.3-symbolic-algebra-ex.rkt")

(module+ test
  (define var 'x)
  (define term-list-1 (list
                       (list 5 (make-scheme-number 1))
                       (list 0 (make-scheme-number (- 1)))))
  (define term-list-2 (list
                       (list 2 (make-scheme-number 1))
                       (list 0 (make-scheme-number (- 1)))))

  (define p1 (make-polynomial var term-list-1))
  (define p2 (make-polynomial var term-list-2))

  (div p1 p2)
  )