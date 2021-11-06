#lang racket
; Exercise 1.16: Design a procedure that evolves an itera-
; tive exponentiation process that uses successive squaring
; and uses a logarithmic number of steps, as does fast-expt .
; (Hint: Using the observation that (b n/2 ) 2 = (b 2 ) n/2 , keep,
; along with the exponent n and the base b, an additional
; state variable a, and define the state transformation in such
; a way that the product ab n is unchanged from state to state.
; At the beginning of the process a is taken to be 1, and the
; answer is given by the value of a at the end of the process.
; In general, the technique of defining an invariant quantity
; that remains unchanged from state to state is a powerful
; way to think about the design of iterative algorithms.)


; Problem: calculating b^n in a number of steps logarithmic of n.
; idea: if n is even, we can calculate
; (b^n/2)^2
; = (b^2)^n/2.

(define (is-even? x)
  (= 0 (remainder x 2)))

; Idea for exp-iter:
; If n = 0, return a
; If n is even, square b and divide n by 2
; Else, set a to a*b, decrement n
(define (exp-iter b n)
  (define (helper a b n)
    (cond
      ((= 0 n) a)
      ((is-even? n)
       (helper a (* b b) (/ n 2)))
      (else
       (helper (* a b) b (- n 1)))))
  (helper 1 b n))

(exp-iter 2 0)
(exp-iter 2 2)
(exp-iter 2 3)
(exp-iter 2 4)
(exp-iter 2 7)
(exp-iter 2 8)
(exp-iter 2 9)
(exp-iter 2 10)