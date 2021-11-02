#lang sicp

; Correct version of count pairs.

; We will obviously need a structure where we can
; add pairs to the list and check if a pair
; is already in the list.

(define (make-pairs-list) nil)

(define (contains-pair? p pairs-list)
  (cond
    ((null? pairs-list) #f)
    ((eq? p (car pairs-list)) #t)
    (else (contains-pair? p (cdr pairs-list)))))

(define (add-pair p pairs-list)
  (append pairs-list (list p)))

; Counting the number of pairs in a list.
; If the top level pair is already in the list,
; we don't have to count it anymore.

; If it is, the count is
; 1 plus the number of pairs in the left and
; the number of pairs in the right.

; x is a list structure.
(define (count-pairs x)
  ; l is the current list, counted-pairs contains the
  ; already counted pairs.
  (let ((counted-pairs (make-pairs-list)))
    (define (helper l)
      (cond
        ((not (pair? l)) 0)
        ((contains-pair? l counted-pairs) 0)
        (else
         ; This current pair was not counted,
         ; so we need to add it to the counted pairs.
         (begin
           (set! counted-pairs (add-pair l counted-pairs))
           (+ 1
              (helper (car l))
              (helper (cdr l)))))))
    (helper x))
  )

(#%provide count-pairs contains-pair? add-pair)