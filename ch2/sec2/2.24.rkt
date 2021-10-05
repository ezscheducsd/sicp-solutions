#lang racket

(require "2.2.2-tree.rkt")

(define t (list 1 (list 2 (list 3 4))))

; According to the relationship between list and cons,
; t is equivalent to:

(cons 1
      (cons (list 2 (list 3 4))
            null))

; Which is equivalent to:

(cons 1
      (cons (cons 2
                  (cons (list 3 4)
                        null))
            null))

; Which is equivalent to:

(cons 1
      (cons (cons 2
                  (cons (cons 3
                              (cons 4
                                    null))
                        null))
            null))

; This is a subtree s1 with a left leaf 2 and a right subtree s2.
; s2 is a tree with a left subtree s3 and an empty right subtree.
; s3 is a tree with left leaf 2 and right subtree s4.
; s4 is a tree with left subtree s5 and an empty right subtree.
; s5 is a tree with left leaf 3 and a right subtree s6.
; s6 is a tree with left leaf 4 and an empty right subtree.

; Should be 4 leaves in total.



(module+ test
  (count-leaves t))