#lang racket
; fibonacci: 
; fib(0) = 0
; fib(1) = 1
; fib(n) = fib(n-1) + fib(n-2)

; Iteration: decrement n, update x and y.
; Keep the current term in y
; Return the current term when n is 0.
(define (fib n)
    (define (fib-iter x y n)
        (if (= n 0)
            y
            (fib-iter (+ x y) x (- n 1))))
    (fib-iter 1 0 n))


(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)


; Trace:
; (fib 0)
; (fib-iter 1 0 0)
; 0

; (fib 1)
; (fib-iter 1 0 1)
; (fib-iter 1 1 0)
; 1

; (fib 3)
; (fib-iter 1 0 3)
; (fib-iter 1 1 2)
; (fib-iter 2 1 1)
; (fib-iter 3 2 0) 