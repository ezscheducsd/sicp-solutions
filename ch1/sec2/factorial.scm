#lang racket
; recursive and iterative implementations of factorial function
; n is 0 or greater.
(define (fact-rec n)
    (if (or (= n 0) (= n 1))
        1
        (* n (fact-rec (- n 1)))))


; fact-iter helper is the helper recursive function
; for fact-iter.
; It keeps track of the current product and the current n.
; Multiply n all the way down to 1.
; Terminate if n is 0 or 1.
; Otherwise, multiply current n times current product, decrease n by 1.
(define (fact-iter n)
    (define (fact-iter-helper cur-prod cur-n)
        (if (or (= cur-n 0) (= cur-n 1))
            cur-prod
            (fact-iter-helper (* cur-prod cur-n) (- cur-n 1))))
    (fact-iter-helper 1 n))


(fact-rec 0)
(fact-rec 1)
(fact-rec 5)

(fact-iter 0)
(fact-iter 1)
(fact-iter 5)