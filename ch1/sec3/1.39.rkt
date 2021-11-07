#lang racket

(require "1.37.rkt")
(require sicp-helpers/mathlib)


; Look at the SICP textbook for the
; Ni and Di functions.
(define (tan-cf x k)
  (define (N i)
    (if (= 1 i)
        x
        (- (square x))))
  (define (D i)
    (+ 1.0 (* 2.0 (- i 1))))
  (cont-frac N D k))

(module+ test
  (define (tan x)
    (tan-cf x 100))
  (tan 2)
  (tan 3))