#lang racket

; An alternative representation of cons, car, cdr

; cons takes two parameters x and y.
; cons returns a function f.
; f takes a function m and applies it to x and y.
(define (cons x y)
  (lambda (m) (m x y)))

; car takes a function z (z is a cons construct)
; To fit the definition of cons:
; car should get the first element x in cons.
; To do this, we can apply the result of
; (cons x y) to a function m that, given two parameters
; p and q, returns p.
(define (car z)
  (z (lambda (p q) p)))

; The same holds for cdr.
(define (cdr z)
  (z (lambda (p q) q)))

(module+ test
  (require rackunit)
  (define pair (cons 1 2))
  (check-eq? (car pair) 1)
  (check-eq? (cdr pair) 2))