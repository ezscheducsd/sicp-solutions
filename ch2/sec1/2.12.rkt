#lang racket

; This is an interval implementation in terms of
; center and width

(require "2.1.4-interval.rkt")

; Center is the center of the interval to make.
; tolerance is the percentage (0-100) tolerance.
(provide make-center-percent)

(define (make-center-percent center tolerance)
  (let ((width (abs (* tolerance center))))
    (make-interval
     (- center width)
     (+ center width))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(module+ test
  (require rackunit)
  (define test-interval (make-center-percent 10 0.25))
  (display-interval test-interval))