#lang racket

(require sicp-helpers/mathlib)

(define (iterative-improve good-enough? improve-guess)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve-guess) (improve-guess guess)))))

; sqrt procedure of 1.1.7:
; improve guess: avg(y + x/y)
; good enough: guess and next guess within a certain threshold
(define (sqrt x)
  (define threshold 0.00001)
  (define (improve-guess guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) threshold))
  ((iterative-improve good-enough? improve-guess) 1))