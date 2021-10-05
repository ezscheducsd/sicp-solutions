#lang racket

; lists are equal if they contain equal elements arranged
; in the same order

; we an define equal? recursively
; in terms of basic eq? equality of symbols.

; a and b are equal if they are both symbols
; and the symbols are eq?
; OR
; they are both lists such that
; (car a) is equal? to (car b)
; and (cdr a) is equal? to (cdr b)
(define (my-equal? a b)
  ; both are not populated lists
  ; so either null or something else
  (cond ((and (not (pair? a)) (not (pair? b)))
         (eq? a b))
        ((and (list? a) (list? b))
         (and (my-equal? (car a) (car b))
              (my-equal? (cdr a) (cdr b))))
        (else #f)))

(module+ test
  (my-equal? 1 2))