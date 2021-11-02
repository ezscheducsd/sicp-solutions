#lang r5rs

; Consider the following make-cycle procedure
; which uses the last-pair procedure defined in Exercise 3.12:

; Given a list x:
; Get the last pair of x, then set its cdr to x.
; This will make last pair of x have a cdr that is
; x, or the first pair in x
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; last pair
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define z (make-cycle (list 'a 'b 'c)))

; If we try to compute (last-pair z), then we will just get
; infinite recursion because there is no end to this list.
; It is a cycle.
(last-pair z)