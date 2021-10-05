#lang racket

; Extend the polynomial system to include subtraction of polynomials
; You may find it helpful to define a generic negation operation.

; So the idea is that if we want to add two polynomials p1 and p2,
; we can first negate p2 then add that to p1.
; But to negate p2, we will have to negate all the coefficients.

; So not only do we need to define a negate procedure for polynomials,
; but also for rationals, complex, and scheme numbers.

(require "2.5.2-generic-arithmetic-raise.rkt")
(require "2.5.3-symbolic-algebra-ex.rkt")

(module+ test
  (require rackunit)
  (define r (make-rational 1 2))
  (define s 3)
  (define c (make-complex-from-real-imag 3 3))
  (check-true (=zero? (add r (negate r))))
  (check-true (=zero? (add s (negate s))))
  (check-true (=zero? (add c (negate c))))

  (define term-list (list
                     (list 1 r)
                     (list 2 c)
                     (list 3 s)))
  (define term-list-2 (list
                       (list 1 c)
                       (list 2 s)
                       (list 3 r)))
  (define var 'x)
  (define p1 (make-polynomial var term-list))
  (define negate-p1 (negate p1))
  (check-true (=zero? (sub p1 p1)))

  (define p2 (make-polynomial var term-list-2))
  (add p1 p2)
  )