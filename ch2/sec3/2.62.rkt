#lang racket

; O(n) implementation of union-set for sets represented as ordered lists.
; We can think of the intersection code and try to apply
; the same idea here.

; Base case: If either set is null,
; then the union is just the other set.

; If 
; Let the first two elements of each set be x1 and x2.
; If they are equal, then they are in the union.
; But we only need one instance
; Add x1 to the union of the union of the cdrs.

; If x1 < x2.
; We want to preserve order in the union.
; Because x1 < x2, x1 is the current smallest
; element between both sets
; First, get the union of cdr set1 and set2,
; then add x1 to that union.

; same case if x1 > x2

(provide union-set-ordered)

(define (union-set-ordered set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    ((= (car set1) (car set2))
     (cons (car set1) (union-set-ordered (cdr set1) (cdr set2))))
    ((< (car set1) (car set2))
     (cons (car set1) (union-set-ordered (cdr set1) set2)))
    (else
     (cons (car set2) (union-set-ordered set1 (cdr set2))))))

(module+ test
  (union-set-ordered null (list 1 2 3))
  (union-set-ordered (list 1 2 3) null)
  (union-set-ordered (list 1 2 3) (list 4 5 6))
  (union-set-ordered (list 4 5 6) (list 1 2 3))
  (union-set-ordered (list 1 3 5 6 8 9) (list 2 4 6 8 10))
  )