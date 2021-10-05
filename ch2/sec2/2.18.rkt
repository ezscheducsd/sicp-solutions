#lang racket

; Idea to reverse a list:
; If the list is a single element, just
; return the same list.
; Otherwise, we want to make the first element
; come last in the new list.

; iterative version
; helper iterator
; - starts with the reverse list as the empty list
; - cons the first element of the current list with
; the reverse list.
; continue with the rest of the original list until it is empty.
(provide reverse)

;(define (reverse l)
;  (define (helper l r)
;    (if (null? l)
;        r
;        (helper (cdr l) (cons (car l) r))))
;  (helper l null))


; Recursive version:
; if null, return itself.
; otherwise, the reverse is the
; reverse of the rest of the list
; appended to the list
; containing only the first element
(define (reverse l)
  (if (null? l)
      null
      (append (reverse (cdr l))
              (list (car l)))))


(module+ test
  (reverse null)
  (reverse (list 1))
  (reverse (list 1 2 3 4 5)))