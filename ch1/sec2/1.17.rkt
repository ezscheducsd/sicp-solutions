#lang racket

(require rackunit)

; Integer multiplication through repeated addition.
; A simple multiplication that is linear in b:
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (is-even? x) (= 0 (remainder x 2)))

; Idea behind logarithmic multiplication in terms of addition:

; a * b = 0 if b = 0
; a * b = (double a) * (halve b) if b is even
; a * b = a * b-1 + a if b is odd
(define (fast-mult a b)
  (cond
    ((= b 0) 0)
    ((is-even? b)
     (fast-mult (double a) (halve b)))
    (else
     (+ a (fast-mult a (- b 1))))))

; Question: can we make this an iterative process?

(module+ test
  (check-eq? (fast-mult 3 4) (* 3 4))
  (check-eq? (fast-mult 3 0) 0)
  (check-eq? (fast-mult 3 124) (* 3 124))
  (check-eq? (fast-mult 5 1000) (* 5 1000))
  )