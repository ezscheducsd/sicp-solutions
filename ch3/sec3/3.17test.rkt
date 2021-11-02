#lang sicp

(#%require "3.17.rkt")
(#%require rackunit)

; list structure with 3 pairs, count is 3.
; top level is a pair.
; car of top level is a pair (1 2).
; cdr of top level is a pair (3 null)
(check-eq? (count-pairs (list (cons 1 2) 3)) 3)

; list structure with 3 pairs, count is 3.
(define y (cons 3 nil))
(define top (cons (cons 1 y) y))
(check-eq? (count-pairs top) 3)

; list structure with3 pairs, count is 7.
(define end (cons 1 nil))
(define second (cons end end))
(define third (cons second second))
(check-eq? (count-pairs third) 3)


; list structure with 3 pairs, count-pairs never terminates.
; we just need a cycle in the list.
(define i (cons 1 nil))
(define j (cons 2 nil))
(define k (cons i j))
(set-cdr! j k)
; this should be an infinite loop
(check-eq? (count-pairs k) 3)