#lang racket
(define (even? x)
    (= 0 (remainder x 2)))

(define (expmod base exp m)
    (cond   ((= exp 0) 1)
            ((even? exp)
             (remainder 
                (square (expmod base (/ exp 2) m))
                m))
            (else
             (remainder 
                (* base (expmod base (-exp 1) m))
                m))))

(expmod 1 0 1)


; Another way to find a prime number: 
; >=2, and its smallest divisor is itself.
; Divisor x of y: y % x == 0.
; Test for integers 2...floor(sqrt(y)).

; Helper is a function that finds the smallest Divisor
; starting with integer start.
; Keep checking current divisor until 
; its square is greater than n.

(define (square x)
    (* x x))


(define (smallest-divisor n)
    (define (helper n cur)
        (cond   ( (> (square cur) n) n ) 
                ( (= 0 (remainder n cur)) cur)  
                ( else (helper n (+ cur 1))) ))
    (helper n 2))


(smallest-divisor 2)
(smallest-divisor 3)
(smallest-divisor 10)
(smallest-divisor 15)
(smallest-divisor 563)