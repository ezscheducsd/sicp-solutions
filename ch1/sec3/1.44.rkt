#lang racket

(require sicp-helpers/mathlib)
(require "1.43.rkt")

(define dx 0.0001)
(define (avg-3 x y z)
  (/ (+ x y z) 3.0))
(define (smooth f)
  (lambda (x)
    (avg-3 (f (- x dx))
           (f x)
           (f (+ x dx)))))

; N-fold smooth function
; so (n-fold-smooth f n)
; return a function
; x -> smooth^n(x)

; How to express this in terms of smooth and repeated?
; remember, repeated takes a function f,
; returns the function
; x -> f^n(x)
; 
(define (n-fold-smooth f n)
  (let ((smooth-f (smooth f)))
    ((repeated smooth n) f)))

(module+ test
  (define smooth-square (smooth square))
  (smooth-square 7)

  (define n 5)
  (define n-smooth-square (n-fold-smooth square 5))
  (n-smooth-square 7)
  )