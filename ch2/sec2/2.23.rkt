#lang racket

; recursive implementation of for each.
; we are not actually returning anything of value
; (the return value can be arbitrary.)
; we are just applying the given procedure
; to each of the elements, and nothing more.
; Here, we will arbitrarily return true.

; If items is null, do nothing
; otherwise, apply proc to the first item,
; then do for each on the rest of the list.
(define (for-each proc items)
  (when (not (null? items))
    (proc (car items)))
  (when (not (null? items))
    (for-each proc (cdr items))))

(module+ test
  (define x (for-each (lambda (x) (newline) (display x))
                      (list 57 321 88)))
  (display x))