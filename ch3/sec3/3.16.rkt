#lang sicp

; procedure to count the number of pairs in any list structure

; naive implementation:
; the number of pairs in any structure is the number in the car
; plus the number in the cdr plus one more to count the current pair.

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; count 3

(#%provide count-pairs)