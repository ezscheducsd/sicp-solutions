#lang racket

(require "2.5.1-generic-arithmetic.rkt")

(module+ test
  (require rackunit)
  (check-true (=zero? 0))
  (check-false (=zero? (make-scheme-number 2)))

  (check-true (=zero? (make-rational 0 4)))
  (check-false (=zero? (make-rational 1 2)))

  (check-true (=zero? (make-complex-from-real-imag 0 0)))
  (check-false (=zero? (make-complex-from-real-imag 0 1)))
  (check-true (=zero? (make-complex-from-mag-ang 0 2)))
  (check-false (=zero? (make-complex-from-mag-ang 1 0)))
  )