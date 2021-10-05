#lang racket

(provide element-of-set-ordered? intersection-set-ordered)

; We don't have to scan the rest of the set
; if the current element is already bigger than
; the target. Because the rest are also going to be
; bigger.
(define (element-of-set-ordered? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set-ordered? x (cdr set)))))

; more impressive speedup with intersection-set
; Idea:

; THIS assumes that elements are unique
(define (intersection-set-ordered set1 set2)
  ; Intersection of two null sets is null
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        ; Get the first element of each set.
        ; If they are equal, then they are definitely in
        ; the intersection.
        ; Get the intersection of the cdrs, then combine
        ; with the first element
        (cond ((= x1 x2)
               (cons x1 (intersection-set-ordered (cdr set1)
                                          (cdr set2))))
              ; If x1 < x2
              ; We know that all the elements in set2
              ; are greater than x1, so x1 is definitely NOT
              ; in the intersection
              ((< x1 x2)
               (intersection-set-ordered (cdr set1) set2))
              ; If x2 < x1
              ; We know that all the elements in set1
              ; are greater than x2, so x2 is definitely NOT
              ; in the intersection
              ((< x2 x1)
               (intersection-set-ordered set1 (cdr set2)))))))