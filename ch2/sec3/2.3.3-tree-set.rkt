#lang racket

(provide element-of-set?
         adjoin-set
         entry
         left-branch
         right-branch
         make-tree)

; Entry is at the first position
(define (entry tree) (car tree))
; Left tree at second position
(define (left-branch tree) (cadr tree))
; Right tree at third position
(define (right-branch tree) (caddr tree))
; Make a tree rooted with entry, plus a left and right tree
(define (make-tree entry left right)
  (list entry left right))

; See if x is in the binary tree
; If the tree is null, then it is NOT in the tree
; Get the root of the current tree
; If root = x, then true.
; If x < root, check left tree
; If x > root, check right tree
(define (element-of-set? x set)
  (cond
    ((null? set) #f)
    ((= x (entry set)) #t)
    ((< x (entry set))
     (element-of-set? x (left-branch set)))
    (else
     (element-of-set? x (right-branch set)))))

; Adjoining an item is implemented similarly and also requires
; log(n) steps
; If inserting into a null tree, make a tree
; with the entry and two null subtrees
; If root = x, just return the same tree
; If x < root, make a new tree with
; the same root, the same right tree,
; and the left subtree with x inserted
; Same case if x > root
(define (adjoin-set x set)
  (cond
    ((null? set)
     (make-tree x null null))
    ((= x (entry set)) set)
    ((< x (entry set))
     (make-tree (entry set)
                (adjoin-set x (left-branch set))
                (right-branch set)))
    (else
     (make-tree (entry set)
                (left-branch set)
                (adjoin-set x (right-branch set))))))

(module+ test
  (define s1 null)
  (define s2 (adjoin-set 10 s1))
  s2
  )