#lang racket

;(deriv g) is the function returning the derivative
;of g at x.

(define dx 0.00001)

(require "1.3.3-fixed-point.rkt")
(require "1.3.4-average-damp.rkt")
(require sicp-helpers/mathlib)

(provide newtons-method)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

;Newton's method of finding the root of a function g:
;Find a fixed point of the mapping x -> x - g(x)/Dg(x)
; remember, fixed-point procedure finds a fixed point
; of a given function given a first guess.

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; Find the square root of x.
; Find a y such that x - y^2 = 0.
; So, find a root of the mapping y -> x - y^2
;(define (sqrt x)
;  (newtons-method
;   (lambda (y) (- x (square y)))
;   1.0))

; General methods: finding the fixed-point of
; a transformation of of a function g.

; Square root with average-damp:
; Original: find a fixed point of y -> x/y
; average damp: find a fixed point of average-damp above function.
(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (/ x y))
   average-damp
   1))