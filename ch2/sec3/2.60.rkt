#lang racket

; Set with duplicates
; Union-set: simply append the two sets
; adjoin-set: simply add the element
; element-of-set?: same

(define (union-set s1 s2) (append s1 s2))

(define (adjoin-set x set) (cons x set))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? (car set) x) #t)
        (else (element-of-set? x (cdr set)))))


; Intersection set is the hardest operation certainly
; Suppose we have an element e11 of s1 and another set s2.

; If e11 occurs no times in s2, then e11 should not be
; in the intersection
; If e11 occurs at least once in s2,
; then let x be the number of occurrences in s1
; y is the number of occurrences is s2
; the number of occurrences of e11 in the intersection
; should be min(x, y)

; If the element is already found, then just return
; the same set (don't want to remove again)
; If the element is not yet found:
; if first element is equal to the 
(define (without-first-occurrence x set)
  (define (helper set removed-first?)
    (if (null? set)
      set
      (cond (removed-first? set)
            ; First occurrence not yet removed
            ; Found the first occurrence. Remove it.
            ((equal? x (car set))
             (helper (cdr set) #t))
            ; Found an occurrence past the first.
            ; We want to keep it
            (else
             (cons (car set)
                   (helper (cdr set) #f))))))
  (helper set #f))

; Recursive strategy:
; Iterate through the elements of set1
; For the current element e1, check if it is in set2.
; If e1 is not in set2:
;   Just get the intersection of (cdr set1) and set2
; If e1 IS in set2:
;   Remember, we want e1 to occur the same amount of times
;   as in the set with less occurrences
;   First, we can get the intersection of (cdr set1)
;   and set2 without the first occurrence of set2.
;   Why? well, assuming that the recursive call works,
;   It will be the intersection with one less occurrence of e1.
;   Then we will just add e1 to that set to account for one more match
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) null)
        ; If the first element is in set2?
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set
                           (cdr set1)
                           (without-first-occurrence (car set1) set2))))
        ; Not in set2? 
        (else
         (intersection-set (cdr set1) set2))))

(module+ test
  (define s (list 2 1 2 3 1 1 1 2 4))
  (without-first-occurrence 4 s)
  (define occur2 (list 1 2 3 4 5 2))
  (define occur5 (list 2 7 8 2 4 4 2 1 2 2))
  (intersection-set occur5 occur2)
  (intersection-set occur2 occur5)
  (intersection-set (list 3 3 3) (list 1 2 3))
  )