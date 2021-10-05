#lang racket

; Lookup in a binary tree:
; This is similar to insert.

; If tree is empty, return false
; Check the root, if equal to key
; If key < root, search in left subtree
; If key > root, search in right subtree

; Note: We have not yet implemented
; a solution where the elements of the set are
; records (key, value pair). We just have keys
; But the idea is similar.

; Instead of comparing just the elements directly,
; we can compare the keys

(require "2.3.3-tree-set.rkt")

(define (lookup key tree-set)
  (cond
    ((null? tree-set) #f)
    ((= key (entry tree-set)) #t)
    ((< key (entry tree-set))
     (lookup key (left-branch tree-set)))
    (else
     (lookup key (right-branch tree-set)))))

(module+ test
  (define t1
    (adjoin-set
     2
     (adjoin-set
      7
      (adjoin-set
       12
       (adjoin-set
        18
        (adjoin-set
         5
         (adjoin-set
          15
          (adjoin-set 10 null))))))))
  (require rackunit)

  (check-true (lookup 12 t1))
  (check-true (lookup 15 t1))
  (check-true (lookup 2 t1))
  (check-false (lookup 1 t1))
  )