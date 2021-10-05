#lang racket
(define (false-undefined b)
    (cond (b 1)))

(false-undefined true)

(and true (/ 3 0))