#lang racket

; How should the difference of two intervals be computed?
(require "2.1.4-interval.rkt")

(module+ test
  (require rackunit)
  (define i1 (make-interval 2 5))
  (define i2 (make-interval -3 3))
  (define d (sub-interval i1 i2))
  (check-eq? (lower-bound d) -1)
  (check-eq? (upper-bound d) 8)
  )