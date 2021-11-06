#lang racket

(require sicp-helpers/mathlib)

(define (square-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (square tree))
        (else (cons
               (square-tree (car tree))
               (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (cond ((null? tree)
         null)
        ((not (pair? tree))
         (square tree))
        (else
         (map square-tree tree))))

(module+ test
  (square-tree-map
   (list 1
         (list 2 (list 3 4) 5)
         (list 6 7)))
  ;(1 (4 (9 16) 25) (36 49))
  )