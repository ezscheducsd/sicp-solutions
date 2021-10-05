#lang racket

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (display first-guess)
  (display "***")
  (try first-guess))

(module+ test
  (require sicp/mathlib)
  (define (f x)
    (/ (log 1000) (log x)))
  (define (f-damp x)
    (average x (/ (log 1000) (log x))))
  (fixed-point f 2)
  (fixed-point f-damp 2))