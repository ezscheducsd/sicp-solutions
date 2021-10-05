#lang racket

; Deep reverse:
; takes a list as an argument.

; Cases:
; Empty list - empty list
; Single element, not list or null - just that element
; List: reverse of the rest of the list, followed by
; the reverse of the first element.

(require "2.18.rkt")

; Idea: first, reverse at the top level of the list.
; (shallow.)
; Then, going through the list, deep reverse each element
; if it is itself a list.
; FIXME: there should be a better way to do this one.

; If it is not a pair (null or atom), return the same.
; If it is a pair, map each atom to the deep reverse.
; Then reverse this map.
;(define (deep-reverse l)
;  (if (not (pair? l))
;      l
;      (reverse (map deep-reverse l))))

; Deep reverse without the help of
; the reverse (shallow procedure):
; So the same idea still holds true.
; We will want to reverse things
; first at the deeper level, then
; reverse the top level elements (shallow).
(define (deep-reverse l)
  (if (null? l)
      null
      ; l is a list.
      ; Get the first element.
      ; If it is not a list, then the result is
      ; just the reverse of the rest of the list
      ; appended to the list containing only
      ; the one element.
      ; If it is a list, then the result is
      ; the reverse of the rest of the list appended
      ; to the list containing the reversed current element.
      (if (pair? (car l))
          (append (deep-reverse (cdr l))
                  (list (deep-reverse (car l))))
          (append (deep-reverse (cdr l))
                  (list (car l))))))

(module+ test

  (define x (list (list 1 2) (list 3 4) (list (list 1 2) (list 3 4))))
  x
  ;((1 2) (3 4))
  (reverse x)
  ;((3 4) (1 2))
  (deep-reverse x)
  ;((4 3) (2 1))

  )