#lang racket

; install =zero? for polynomials in the generic arithmetic package.
; See 2.5.3-symbolic-algebra.

; Idea: a polynomial is zero if its term list is the empty list
; or every coefficient in its term list is 0.

; See 2.5.3-symbolic-algebra-ex.rkt

(require "2.5.3-symbolic-algebra-ex.rkt")
(require "2.5.1-generic-arithmetic.rkt")

(module+ test
  (require rackunit)
  (define p-termlist (list (list 3 (make-rational 0 2))
                           (list 2 (make-scheme-number 0))
                           (list 1 (make-complex-from-mag-ang 0 3))))
  (define var 'x)
  (define p (make-polynomial var p-termlist))
  (check-true (=zero? p))
  )