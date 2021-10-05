#lang racket

(require "2.3.3-tree-set.rkt")

(provide tree->list-1)

; Tree to list first version
(define (tree->list-1 tree)
  (if (null? tree)
      ; A null tree as a list is just a null list
      '()
      ; First, convert the left subtree to a list of its own
      ; Then, convert the right subtree to a list of its own
      ; Then make a list out of the two with the current
      ; entry in between
      ; This preserves the order because the entry
      ; is in the right position in the list relative
      ; to the other elements.
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))
; tree->list-1.
; Base case: a null list is in order
; Suppose that tree->list-1 works for tree up to size k.
; A tree of size k+1 consists of a node
; with up to 2 subtrees with a max size of k
; We so tree->list-l will certainly work on both
; subtrees, meaning the left and right lists will
; be in order
; We add the entry by appending it in front of the left
; list, then appending the right list.
; This is also in order because the left list
; consists of entries from the left subtree (which are all)
; less than the current entry
; The right list consists of entries from the right subtree
; (which are all greater than the current entry)
; And the way in which we combine the lists produces another
; in order list.

; For a balanced tree of size n,
; tree->list-1 takes TL(floor(N/2)) + TL(floor(N/2)) + 1.
; This will take about n in time.

; Tree to list second version
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        ; If the tree is null, then we are done copying
        ; Just return the result
        result-list
        ; Otherwise, we:
        ; Copy the right branch to the result list
        ; to get r2
        ; Add the entry to the beginning of r2 to get r3
        ; Then copy the left branch to r3
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))
; tree->list-2
; Base case:
; The null list represents the null tree in order
; From inspection, it looks like both procedures will produce the same result.
; but is this the case?

; From testing, it also appears to be the case

; How do we know it is completely correct?
; Claim: Both make lists in order.

; Do the two procedures have the same order of growth?


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
  (tree->list-1 t1)
  (tree->list-2 t1)
  )