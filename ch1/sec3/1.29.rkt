#lang racket

;Simpson's rule:
;h/3 times sum.
;sum first term a

(require "general_sum.rkt")
(require sicp/mathlib)

;Simpson's rule for integral approximation (i to j.)
;First, try to get only h/3 * (y0 + y1 + ... + yn)
;In this case, a is 0. b is 1000. h is (b-a)/n.
;Current term partial is f(h*a + i).
;If a is 0 or b, leave alone.
;If even, multiply by 2.
;If odd, multiply by 3.

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (next cur) (+ cur 1))
  (define (term x)
    (define partial (f (+ a (* x h))))
    (cond ((or (= x a) (= x b)) partial)
          ((even? x) (* 2 partial))
          (else (* 4 partial))))
  (* (/ h 3.0) (sum term 1 next n)))


(module+ test
  (simpson-integral cube 0 1 1000))