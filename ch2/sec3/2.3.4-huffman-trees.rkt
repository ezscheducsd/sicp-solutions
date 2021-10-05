#lang racket

(provide make-leaf
         leaf?
         symbol-leaf
         weight-leaf
         make-code-tree
         left-branch
         right-branch
         symbols
         weight
         decode
         choose-branch
         adjoin-set
         make-leaf-set)

; Huffman encoding trees
; Leaves are represented by a list consisting of the 'leaf symbol,
; the symbol (character), and the weight

; Constructor, checker, and selector
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; General tree: list containing left branch, right branch,
; set of symbols, and a weight

; When we merge two nodes to make a tree
; the tree gets the two branches respectively
; The symbols is the union of the lists of the left and right node
; The weight is the sum of their weights too
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch
         tree) (car
                tree))
(define (right-branch tree) (cadr tree))

; Symbols of a leaf is just the list containing the one symbol for
; that leaf
; Otherwise, it is the third entry in the tree
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

; Weight of a leaf is the frequency of its symbol
; Otherwise, it is the 4th entry of the tree
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; Decoding
(define (decode bits tree)
  ; decode-1 returns a list of symbols
  (define (decode-1 bits current-branch)
    (if (null? bits)
        ; no more bits - no symbols
        '()
        ; get the next branch to traverse down
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          ; If we found a leaf, we have the next character.
          ; Get the list of characters for the remaining bits,
          ; then append this character in front
          ; Remember, we start again from the top of the tree
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              ; No leaf found yet, get the list for the remaining
              ; bits starting at the next branch
              (decode-1 (cdr bits) next-branch)))))
  ; Get the list of characters starting from the root
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

; To help the huffman tree construction
; We need a set of leaves and trees
; They are ordered by their weights (increasing)
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; Given a list of symbol-frequency pairs, construct an intial ordered
; set of leaves, ready to be merged
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    ; symbol
                    ; frequency
                    (make-leaf-set (cdr pairs))))))