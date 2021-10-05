#lang racket
(define (loop x)
    (loop (cons x x)))

(loop 2)