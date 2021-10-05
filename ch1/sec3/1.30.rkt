#lang racket

;Iteration of general sum procedure.

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (test-sum-integer a b)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (sum identity a inc b))

(module+ test
  (test-sum-integer 1 10))