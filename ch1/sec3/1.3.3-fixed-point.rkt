#lang racket

;Fixed point: a point x of a continuous function
;such that f(x) = x.
;
;The idea: Given a current x, compute f(x) (our next guess)
;If difference of x and guess are within a certain threshold,
;then accept. Otherwise, try the next guess.

(provide fixed-point fixed-point-of-transform)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(module+ test
  (define (golden-fixed-func x)
    (+ 1.0 (/ 1 x)))
  (fixed-point golden-fixed-func 1))