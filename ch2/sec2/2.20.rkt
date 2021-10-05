#lang racket

; dotted-tail notation.
; arbitrary number of arguments after the .
; is a list in the last argument.

; recursive definition:
; if the current list is empty, return empty.
; if the current element has the same
; parity as the first element,
; return a list of the current element
; and the same parity elements of the remaining list.

(define (even x)
  (= 0 (remainder x 2)))

(define (odd x)
  (not (even x)))

; Recursive idea:
; first, get the parity of the first integer.
; 
(define (same-parity . ints)
  (define same-parity-first? (if (even (car ints)) even odd))
  (define (helper cur-l)
    (if (null? cur-l)
        null
        (let ((first (car cur-l))
              (rem (cdr cur-l)))
          (if (same-parity-first? first)
              (cons first (helper rem))
              (helper rem)))))
  (cons (car ints) (helper (cdr ints))))
  
(module+ test
  (same-parity 1 2 3 4 5 6 7)
  (same-parity 2 3 4 5 6 7))