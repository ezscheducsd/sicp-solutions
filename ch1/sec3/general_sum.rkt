#lang racket

(require sicp-helpers/mathlib)

(provide sum)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc x)
  (+ x 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(module+ test
  (sum-cubes 1 10))