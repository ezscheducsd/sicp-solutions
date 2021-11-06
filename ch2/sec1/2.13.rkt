#lang racket

; Under the assumption of small percentage tolerances,
; there is a simple formula for the approximate percentage
; of the product of two intervals in terms of tolerances of
; the factors.
; Assume that all numbers are positive.

; If all the numbers are positive, then the
; lower bound of the product is the product of the two lower bounds.
; The upper bound of the product is the product of the
; two upper bounds.

; i1: (c1 - c1*t1, c1 + c1*t1)
; i2: (c2 - c2*t2, c2 + c2*t2)

; i1 * i2 = (c1c2 - c1c2t2 - c1c2t1 + c1c2t1t2, c1c2 + c1c2t2 + c1c2t1 + c1c2t1t2)
; width: c1c2t1 + c1c2t2
; center: c1c2 + c1c2t1t2

; tolerance ratio: (t1t2)/(1 + t1t2)

(require sicp-helpers/mathlib)
(require "2.1.4-interval.rkt")
(require "2.12.rkt")

(provide interval-center
         interval-width
         interval-tolerance)

(define (interval-center i)
  (average (lower-bound i) (upper-bound i)))

(define (interval-width i)
  (abs (- (lower-bound i) (interval-center i))))

(define (interval-tolerance i)
  (/ (interval-width i) (interval-center i)))

(module+ test
  (define t1 0.3)
  (define t2 0.14)
  (define i1 (make-center-percent 5 t1))
  (define i2 (make-center-percent 15 t2))
  (define i1i2 (mul-interval i1 i2))
  (define exp-tolerance (/ (+ t1 t2) (+ 1 (* t1 t2))))
  (define act-tolerance (interval-tolerance i1i2))
  (display exp-tolerance)
  (newline)
  (display act-tolerance)
  (newline))