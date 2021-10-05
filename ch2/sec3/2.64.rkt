#lang racket

; This procedure converts an ordered list to a balanced binary tree
; partial-tree arguments:
; - integer n
; - list of at least n elements
; - constructs a balanced tree using the first n elements of the list

; Result: pair
; car - constructed tree
; cdr - list of elements not included in the tree

(require "2.3.3-tree-set.rkt")

(provide list->tree)

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      ; Base case
      ; Making a tree of 0 elements is the empty list
      ; None of the elements are used in the tree, so return
      ; the same elements
      (cons '() elts)
      ; Left size if floor( (n-1) / 2 )
      ; Account for the root
      (let ((left-size (quotient (- n 1) 2)))
        ; Get the partial tree and unused elements
        ; for building the left tree
        (let ((left-result
               (partial-tree elts left-size)))
          ; Same
          ; Right size.
          ; We already have left-size used.
          ; Remaning N - left-size
          ; Need one more spot for the root
          ; So right size is N - left-size - 1
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            ; The first element of leftovers from left
            ; tree construction will be the root
            (let ((this-entry (car non-left-elts))
                  ; Build the right subtree from
                  ; the remaining elements and right size
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              ; Unpack tree and remaining elements from
              ; construction of right tree
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                ; Return the built tree and remaining
                ; elements
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

; Tree produced by (1 3 5 7 9 11)
; Root 5
;   Left tree root 1
;     Null left
;     Right tree root 3
;       Null left, Null right
;   Right tree root 9
;     Left tree root 7
;       Null left, null right
;     Right tree root 11
;       Null left, null right

; b. What are the number of nodes in the call tree?
; For size N,
; There are two calls of size N/2
; size 0 has no subcalls
; N subcalls.
; Order of growth is N.


(module+ test
  (define l (list 10 5 7 3 4 1 8 2 6 9))
  (list->tree l)
  )