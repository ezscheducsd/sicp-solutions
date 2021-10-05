#lang racket
; Exercise 1.11: A function f is defined by the rule that
; f(n) = 
; n if n < 3
; f(n-1) + 2f(n-2)+3f(n-3) if n>=3.
; Write a procedure that computes f by means of a recursive
; process. Write a procedure that computes f by means of an
; iterative process.


; The recursive case is easy to implement.
(define (myF n)
    (if (< n 3)
        n
        (+ (myF (- n 1)) (* 2 (myF (- n 2))) (* 3 (myF (- n 3))) )
    ))

(myF 0)
(myF 1)
(myF 2)
(myF 3)
(myF 4)
(myF 5)


; Iterative case: keep four state variables.
; three immediate computations and a count.
; Stop when count is 0.

(define (myF-iter n)
    (define (myF-iter-helper x y z cur-n)
        (if (= cur-n 0)
            z
            (myF-iter-helper    (+ x (* 2 y) (* 3 z))
                                x
                                y
                                (- cur-n 1))))
    (myF-iter-helper 2 1 0 n))

(myF-iter 0)
(myF-iter 1)
(myF-iter 2)
(myF-iter 3)
(myF-iter 4)
(myF-iter 5)