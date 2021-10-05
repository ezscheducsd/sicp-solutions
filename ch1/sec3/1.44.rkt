#lang racket

; smoothed version of a function f:
; the function whose value at point x is the average
; of f(x - dx), f(x), f(x + dx)

(require "1.43.rkt")

(define dx 0.00001)

; returns a procedure that computes
; the smoothed f
(define (smooth f)
  (define (avg x y z)
    (/ (+ x y z) 3.0))
  (lambda (x)
    (avg (f (- x dx)) (f x) (f (+ x dx)))))

; given f, n-folded smooth f is:
; smooth (smooth (smooth (... smooth(f(x))))), with n smooth applications.
; To get the n-folded smooth function,
; idea: repeat smooth to f n times.
(define (n-folded-smooth f n)
  ((repeated smooth n) f))

(module+ test
  (sin 1)
  ((n-folded-smooth sin 1) 1)
  ((n-folded-smooth sin 2) 1)
  ((n-folded-smooth sin 3) 1)
  ((n-folded-smooth sin 5) 1)
  ((n-folded-smooth sin 10) 1))