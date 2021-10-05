#lang racket
; Exercise 1.10: e following procedure computes a math-
; ematical function called Ackermann’s function.
(define (A x y)
(cond ((= y 0) 0)
((= x 0) (* 2 y))
((= y 1) 2)
(else (A (- x 1) (A x (- y 1))))))
; What are the values of the following expressions?
(A 1 10)
(A 0 (A 1 9))



(A 2 4)
(A 3 3)
; Consider the following procedures, where A is the proce-
; dure defined above:

; Give concise mathematical definitions for the functions com-
; puted by the procedures f , g , and h for positive integer val-
; ues of n. For example, (k n) computes 5n^2 .
(define (f n) (A 0 n)) = 2n

(define (g n) (A 1 n)) = 2^n
;g(1) = 2
;g(n), n > 1
;(g 2)
;(A 1 2)
;(A 0 (A 1 1))
;(A 0 (2))
;2 * 2 = 4
;g(2) = 4

;g(3)
;(A 1 3)
;(A 0 (A 1 2))
;(A 0 4)
;2 * 4 = 8
;g(3) = 8
;We can see that g(n) = 2^n
;
;
;(define (h n) (A 2 n)) = 2^2^...^2, where the number of 2's is in.
;h(1) = 2
;(h 1)
;(A 2 1)
;
;h(2)
;(h 2)
;(A 2 2)
;(A 1 (A 2 1))
;2^(A 2 1)
;2^2 = 4
;
;
;h(3)
;(h 3)
;(A 2 3)
;(A 1 (A 2 2))
;(A 1 (A 1 (A 2 1)))
;(A 1 (A 1 2))
;(A 1 2^2)
;2^(2^2)
;2^4
;=16.
;
;(define (k n) (* 5 n n)) = 5n^2
