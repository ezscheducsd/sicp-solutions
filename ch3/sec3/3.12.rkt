#lang r5rs

; sectio 2.21 had the following procedure for
; appending lists
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

; This forms a new list by successively consing elements
; of x onto y.
; We also have append!
; But this is a mutator rather than a constructor.

; appends lists by splicing them together, modifying
; the final pair of x so that its cdr is now y

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

; last pair
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))


; consider
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
; (a b c d)
(cdr x)

; The output here would just be (b)

(define w (append! x y))
; x is originally (a b)
; This makes the cdr of the last pair of x to be y.
; at this point, w is the same as z.
w
; (a b c d)
(cdr x)
; The output here should be (b c d)
; Remember, append! a couple lines before modified the
; cdr of the original x pair.