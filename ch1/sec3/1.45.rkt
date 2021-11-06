#lang racket

(require "1.3.3-fixed-point.rkt")
(require "1.3.4-average-damp.rkt")
(require "1.43.rkt")
(require sicp-helpers/mathlib)

; Finding the fourth root of x:
; Find y such that y^4 = x
; y = x/y^3
; A single average damp is not enough to make this converge.
; But if we average damp twice, it does converge.

; Experimenting to see how many repeated average damps
; are required to compute nth roots as a fixed-point
; based upon average damping y -> x/y^n-1

; 4th root: 2
; 5th root: 2
; 6th root: 2
; 7th root: 2
; 8th root: 3

; By experimentation, we can see that we need
; floor (lg n) average damps for the nth root function.
; Procedure for computing the nth root:
; The untransformed function is y->x/y^n-1
; The transform is (repeated average-damp floor(lg n))
; We can just start with an initial guess of 1.
(define (nth-root n x)
  (fixed-point-of-transform
   (lambda (y) (/ x (fast-expt y (- n 1))))
   (repeated average-damp (floor (log n 2)))
   1))

(module+ test
  (require rackunit)
  (check-eq? (nth-root 4 81) 3)
  (check-eq? (nth-root 10 1024) 2))