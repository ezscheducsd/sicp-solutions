#lang racket

(require sicp-helpers/mathlib)
(require "2.2.1-map.rkt")

; A tree is represented as
; a list of subtrees, where
; each subtree is either:
; a leaf
; an empty tree
; another tree.

; So we can use map somehow.
; The map should take care of the
; cases individually.
; leaf - apply proc
; null - same.
; tree - tree-map.
(define (tree-map proc tree)
  (map
   (lambda (subtree)
     (cond ((null? subtree)
            null)
           ((not (pair? subtree))
            (proc subtree))
           (else
            (tree-map proc subtree))))
   tree))

(define (square-tree tree)
  (tree-map square tree))

(module+ test
  (square-tree
   (list 1
         (list 2 (list 3 4) 5)
         (list 6 7)))
  ;(1 (4 (9 16) 25) (36 49))
  )