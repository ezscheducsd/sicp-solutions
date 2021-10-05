#lang racket

; Sets: abstract data type with the following API:
; union-set
; intersection-set
; element-of-set?
; adjoin-set - add an element to a set

; Unordered lists

(provide element-of-set?)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; If x is already in the set, just return the same set
; Otherwise, return the set with the new element attached
; to the front (arbitrarily.)
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

; Exericse 2.59
; Union set s1 and s2
; Idea: for the current element
; e1 of set1, adjoin it with set2.
; Then we find the union of
; set1 without the first element
; and the adjoined set
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (let ((adjoined-first (adjoin-set (car set1) set2)))
        (union-set (cdr set1) adjoined-first))))


; Intersection of s1 and s2
; Recursive strategy
; If we have the intersection of set2 and cdr set1,
; then we only need to know whether to include
; car set1 in the set.
; But this depends on whether (car set1) is also
; in set2
(define (intersection-set set1 set2)
  ; one is empty - intersection as to be null
  (cond ((or (null? set1) (null? set2)) '())
        ; if current element of set1 is in set2, then
        ; it is certainly in both sets
        ; We can find the intersection of (cdr set1) and set2
        ; which will definitely NOT have the current element.
        ; Then we set the first element according to its presence
        ; in set 2
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


(module+ test
  (define s1 (list 1 2 3))
  (define s2 (list 2 4 6))
  (define s3 (list 1 4 9))
  (union-set s1 (union-set s2 s3))
  )