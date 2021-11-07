#lang racket

(require "2.1.4-interval.rkt")

(module+ test
  (define i1 (make-interval 3 5))
  (define i2 (make-interval -2 2))
  (div-interval i1 i2)
  )