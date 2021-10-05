#lang racket
; Exercise 1.7: The good-enough? test used in computing
; square roots will not be very effective for finding the square
; roots of very small numbers. Also, in real computers, arith-
; metic operations are almost always performed with lim-
; ited precision. This makes our test inadequate for very large
; numbers. Explain these statements, with examples showing
; how the test fails for small and large numbers. An alterna-
; tive strategy for implementing good-enough? is to watch
; how guess changes from one iteration to the next and to
; stop when the change is a very small fraction of the guess.
; Design a square-root procedure that uses this kind of end
; test. Does this work better for small and large numbers?


; Showing that the test may fail for very small numbers.
; The threshold test can be dangerous for small numbers because
; if the number for which we are trying to find the square root 
; is itself very small (much smaller than the threshold, for example)
; then the threshold will allow for answers that are actually way
; off.

; for example, with threshold 1/100, x 1/100 and guess 1/100,
; we get 1/100 as the answer.

; We can see that this also fails for very large numbers.
; I'm having trouble thinking about the problem for large numbers


; Let's define the alternate procedure.

(define fraction (/ 1 1000))

; a guess is good enough if the difference from the current guess
; to the next guess is a small fraction of the current guess.
(define (good-enough guess x)
    (< 
        (abs (- guess (improve-guess guess x)))
        (* fraction guess)))

(define (avg x y)
    (/ (+ x y) 2.0))

; improved guess is the average of the guess and x/guess.
(define (improve-guess guess x)
    (avg guess (/ x guess)))

(define (square-root guess x)
    (if (good-enough guess x)
        guess
        (square-root (improve-guess guess x) x)))


(square-root 10 25)