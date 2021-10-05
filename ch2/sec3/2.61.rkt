#lang racket

(require "2.3.3-ordered-sets.rkt")

(provide adjoin-set)

; adjoin and element to an ordered set
; This will sill take linear time,
; but if random enough, we expect that the
; new element will be smaller than about half
; of the other elements.
; Find the place to insert - this is the place where
; the inserted element is less than the current element
(define (adjoin-set x set)
  (cond
    ; Adjoining to a null set is just the set with one element
    ((null? set) (cons x null))
    ; If the inserted element equals the first element - already in
    ; the set. Just return the same set
    ((= x (car set)) set)
    ; If the new element is less than the first element
    ; Insert the new element before this one.
    ((< x (car set)) (cons x set))
    ; If the new element is greater than the first
    ; return the first element adjoined to
    ; the latter set with the new element inserted.
    (else
     (cons (car set) (adjoin-set x (cdr set))))))

(module+ test
  (define s1 (list 2 4 6))
  (define s2 (list 1 3 5))
  (define insert-first (adjoin-set 0 s1))
  (define insert-second (adjoin-set 3 insert-first))
  (define insert-third (adjoin-set 50 insert-second))
  insert-first
  insert-second
  insert-third
  (adjoin-set 1 null)
  )