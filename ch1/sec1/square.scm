#lang racket
(define (square x)
    (* x x))
(define (sumofsquares x y)
    (+ (square x) (square y)))