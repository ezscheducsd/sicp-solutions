#lang racket

;General product procedure.

(require sicp/mathlib)

(define (prod term a next b)
  (if (> a b)
      1
      (* (term a)
         (prod term (next a) next b))))


;Factorial: product of identity starting from 1 to n
(define (factorial n)
  (prod identity 1 inc n))


; pi/4 approximation.
; terms:
; 1,2, .... n
; if odd: t = (k + 1)/2. 2^t / 1 + 2^t
; if even: t = k/2  2^(t+1) / 1 + 2^t
(define (pi-over-4 n)
  (define (term k)
    (define even? (is-even? k))
    (define m (if even?
                  (/ k 2)
                  (/ (inc k) 2)))
    (define partial (* 2.0 m))
    (if even?
        (/ (+ 2 partial) (inc partial))
        (/ partial (inc partial))))
  (prod term 1 inc n)) 


(module+ test
  (require rackunit)
  (check-eq? 1 (factorial 0))
  (check-eq? 1 (factorial 1))
  (check-eq? 120 (factorial 5)))