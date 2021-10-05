#lang racket

; Use list->tree and tree->list-1 to give O(n)
; implementations of union set and intersection-set
; for sets implemented as balanced binary trees

(require "2.62.rkt")
(require "2.63.rkt")
(require "2.64.rkt")
(require "2.3.3-tree-set.rkt")
(require "2.3.3-ordered-sets.rkt")

; All we have so far is adjoin-set and make tree.

; How to get the union of two trees?
; Well, given t1 and t2 , we can get lists l1 and l2, respectively,
; both of which are in order (from 2.63)
; What we would like to do is combine the lists
; into one ordered list (without duplicates),
; then build a tree from that.
; We CAN combine them - this is union-set ordered from 2.62

(define (union-set-tree tree1 tree2)
  (let ((list1 (tree->list-1 tree1))
        (list2 (tree->list-1 tree2)))
    (let ((union-list (union-set-ordered list1 list2)))
      (list->tree union-list))))

; How to get the intersection of two trees?
; We can do the same thing. But instead, we used intersection-set-ordered
; instead of union-set-ordered

(define (intersection-set-tree tree1 tree2)
  (let ((list1 (tree->list-1 tree1))
        (list2 (tree->list-1 tree2)))
    (let ((intersection-list (intersection-set-ordered list1 list2)))
      (list->tree intersection-list))))