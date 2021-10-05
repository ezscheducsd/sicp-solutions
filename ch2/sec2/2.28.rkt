#lang racket

; fringe: takes a tree as an argument.
; returns a list whose elements are all the leaves
; of the tree arranged in left to right order.

; recursive idea:
; fringe of a leaf is just the list containing the leaf.
; fringe of empty subtree is the empty list.
; fringe of a tree is the fringe of left appended
; to fringe of right.

(define (fringe tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(module+ test
  (define x (list (list 1 2) (list 3 4)))
  (fringe x)
  ; (1 2 3 4)
  (fringe (list x x))
  ; (1 2 3 4 1 2 3 4)
  )