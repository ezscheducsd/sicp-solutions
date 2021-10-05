#lang racket

(require "2.1.4-interval.rkt")

; Center is the center of the interval to make.
; Tolerance (from 0 to 1) is the percentage of tolerance
; (ratio of interval width to interval midpoint.)
(provide make-center-percent)

(define (make-center-percent center tolerance)
  (let ((width (abs (* tolerance center))))
    (make-interval
     (- center width)
     (+ center width))))

(module+ test
  (require rackunit)
  (define test-interval (make-center-percent 10 0.1))
  (display-interval test-interval))