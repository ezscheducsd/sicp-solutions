#lang racket

; See 2.1.1-rational-numbers for the normalization.

(require "2.1.1-rational-numbers.rkt")

(module+ test
  (make-rat -2 3)
  (make-rat 2 -3)
  (make-rat -2 -3)
  (make-rat 2 3)
  )