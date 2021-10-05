#lang racket

; Encode takes a message and a huffman encoding
; tree as arguments and gives the encoded message(define (encode message tree)

(require "2.3.4-huffman-trees.rkt")

(provide encode)

(define (encode message tree)
  (if (null? message)
      ; If the current message is empty,
      ; no bits should be outputted
      '()
      ; Get the bits in the encoding of the symbol
      ; Then append those bits to the encoding of the rest of the tree
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; Given a symbol and a huffman tree,
; return the encoding of the symbol according to the tree.



(require "2.3.3-sets.rkt")

; Select the left branch or the right branch
; depending on which branch contains the symbol in its set.
; If neither contains the symbol, raise an error.
(define (select-bit-branch symbol tree)
  (let ((left (left-branch tree))
        (right (right-branch tree)))
    (cond
      ((element-of-set? symbol (symbols left)) (cons 0 left))
      ((element-of-set? symbol (symbols right)) (cons 1 right))
      ; Fixme: specify which symbol
      (else (error "Huffman tree does not include a symbol.")))))


; Remember the process of getting the encoding:
; Start at the root.
; From the current node, check if the
; symbol is in the le(define sample-tree
; If it is in neither, raise an error.

; Add bit 0 if left branch matches or 1
; if the right branch matches
; If the matching branch is a leaf, then we can stop
; and return the final bit list.
(define (encode-symbol symbol tree)
  (define (helper bits branch)
    (let ((next-bit-branch (select-bit-branch symbol branch)))
      (let ((new-bits (append bits (list (car next-bit-branch)))))
        (if (leaf? (cdr next-bit-branch))
            new-bits
            (helper new-bits (cdr next-bit-branch))))))
  (helper null tree))


(module+ test
  (define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                     (make-leaf 'B 2)
                     (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))
  (decode (encode '(A D A B B C A) sample-tree) sample-tree)
  )