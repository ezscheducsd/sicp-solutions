#lang racket

#|
smallest divisor procedure:
This finds the smallest divisor of a number that
is not equal to 1.
Starting with cur as 2, check if cur is a divisor.
If not, recheck with cur incremented by 1.
If cur^2 is greater than the target number, then
the smallest divisor is just the number iself.
|#

(require sicp-helpers/mathlib)

(define (smallest-divisor x)
  (define (helper cur)
    (cond ((> (square cur) x) x)
          ((= 0 (remainder x cur)) cur)
          (else (helper (+ 1 cur)))))
  (helper 2))

(module+ test
  (require rackunit)
  (check-eq? 3 (smallest-divisor 3))
  (check-eq? 2 (smallest-divisor 10)))