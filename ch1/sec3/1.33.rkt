#lang racket

(require sicp-helpers/mathlib)
(require "../sec2/prime.rkt")

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (helper a b)
    (cond ((> a b) null-value)
          ((filter a)
           (combiner (term a)
                     (helper (next a) b)))
          (else (helper (next a) b))))
  (helper a b))

(define (rel-prime a b)
  (= 1 (gcd a b)))

(define (product-to-n-rel-prime n)
  (define (filter a)
    (rel-prime a n))
  (filtered-accumulate
   *
   1
   identity
   1
   inc
   n
   filter))

(define (sum-prime a b)
  (filtered-accumulate + 0 square a inc b prime?))

(module+ test
  (require rackunit)
  (check-eq? (product-to-n-rel-prime 15) (* 1 2 4 7 8 11 13 14))
  (check-eq? (sum-prime 2 15) (+ 4 9 25 49 121 169)))