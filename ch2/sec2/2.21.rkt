#lang racket

(require "2.2.1-map.rkt")
(require sicp-helpers/mathlib)


(define (square-list items)
  (if (null? items)
      null
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list-map items)
  (map square items))


(module+ test
  (define ints (list 1 2 3 4 5 6))
  (square-list ints)
  (square-list-map ints))