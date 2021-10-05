#lang racket
(define (abs x)
    (cond   ((< x 0) (- x))
            (else x)))

(define (abs-if x)
    (if     (< x 0)
            (- x)
            x))