#lang racket

(require sicp/mathlib)
(require "1.37.rkt")

(define n (lambda (i) 1.0))
(define (three-divides? x)
  (= 0 (remainder x 3)))
(define (d i)
  (if (three-divides? (- i 2))
      (* 2 (inc (quotient i 3)))
      1.0))

(define (approx-e k)
  (+ 2 (cont-frac n d 100)))

(module+ test
  (approx-e 100))