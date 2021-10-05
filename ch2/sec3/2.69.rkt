#lang racket

; Takes a list of symbol-freq pairs, generates
; a huffman tree according to huffman algorithm

(require "2.3.4-huffman-trees.rkt")
(require "2.68.rkt")
(require "2.67.rkt")

(provide generate-huffman-tree)

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; Node set is a list of leaves or trees
; We can access the weight and symbols of leaves and trees.

; To construct a tree, do the following:
; Take the two nodes with the smallest weights
; and remove from set
; Create a new node with the two nodes using make-code-tree
; Insert the new node back into the set
; Repeat until there is only one element in the set.

; The two smallest nodes are at the front of the set (since
; it is ordered)
(define (successive-merge node-set)
  (if (= 1 (length node-set))
      (car node-set)
      (let ((left (car node-set))
            (right (cadr node-set)))
        (let ((new-node (make-code-tree left right)))
          (successive-merge
           (adjoin-set new-node (cddr node-set)))))))

(module+ test
  (define pairs '((A 4) (B 2) (C 1) (D 1)))
  (define sample-tree (generate-huffman-tree pairs))
  (define message '(A B A C D A B))
  (encode message sample-tree)
  (decode (encode message sample-tree) sample-tree)
  )