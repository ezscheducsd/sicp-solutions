#lang racket
; A procedure for finding the square root of a number x is as follows:
; Start with a guess y. If this is close enough, our final answer is y.
; Otherwise, our new guess is the average of y and x/y.

(define threshold (/ 1 1000))

(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
        (else else-clause)))

; a guess is good enough if the square of the guess - the target is less than
; the threshold.
; abs(guess^2 - x) < threshold
(define (good-enough guess x)
    (<  (abs (- x (* guess guess)))
        threshold))

(define (avg x y)
    (/ (+ x y) 2.0))

; improved guess is the average of the guess and x/guess.
(define (improve-guess guess x)
    (avg guess (/ x guess)))

(define (square-root guess x)
    (if (good-enough guess x)
        guess
        (square-root (improve-guess guess x) x)))