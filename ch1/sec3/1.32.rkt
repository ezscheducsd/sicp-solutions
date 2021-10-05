#lang racket

(require sicp/mathlib)

;recursive accumulate.
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner
       (term a)
       (accumulate combiner null-value term (next a) next b))))

; iterative accumulate.
(define (accumulate-iter combiner null-value term a next b)
  (define (helper a b result)
    (if (> a b)
        result
        (helper
         (next a)
         b
         (combiner result (term a)))))
  (helper a b null-value))


(define (sum term a next b)
  (accumulate-iter + 0 term a next b))


(define (prod term a next b)
  (accumulate-iter * 1 term a next b))


(define (factorial-accum n)
  (prod identity 1 inc n))

(define (sum-integer-accum a b)
  (sum identity a inc b))

(module+ test
  (require rackunit)
  (check-eq? (factorial-accum 5) 120)
  (check-eq? (factorial-accum 0) 1)
  (check-eq? (sum-integer-accum 1 10) 55))