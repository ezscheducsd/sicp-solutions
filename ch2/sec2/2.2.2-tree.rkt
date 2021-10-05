#lang racket

; A sequence represented as a tree structure.
; An empty list is an empty tree
; A pair is a subree
; A single element in the list, if not null and not a pair,
; is a leaf.

(provide count-leaves)

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(module+ test
  (define x (cons (list 1 2) (list 3 4)))
  (count-leaves x)
  )