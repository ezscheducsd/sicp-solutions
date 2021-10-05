#lang racket

(require "2.2.3-sequences.rkt")

; count-leaves as accumulate.
; Final result should be the number of leaves.
; Operation: addition?
; What should the sequence be?

; null tree - count 0
; leaf - count 1
; tree - count of all subtrees

; maybe we can use enumerate leaves?
; this is the easy way.
;(define (count-leaves t)
;  (accumulate
;   + 0 (map (lambda (x) 1) (enumerate-tree t) )))

; without enumerate-tree:
; idea: still use + as the op with 0 as the initial.
; if we map each tree to its leaf count, then
; we can just add each current element the sum.

; need to be careful with recursive cases.
; map each subtree to its leaf counts.
(define (count-leaves t)
  (accumulate
   + 0 (map
        (lambda (tree)
          (cond ((null? tree) 0)
                ((not (pair? tree)) 1)
                (else (count-leaves tree))))
        t)))

(module+ test
  (define t (list 1 (list 7 8) 3 (list 1 2 (list 3 4))))
  (count-leaves t)
  )