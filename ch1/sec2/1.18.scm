#lang racket
; Exercise 1.17: The exponentiation algorithms in this sec-
; tion are based on performing exponentiation by means of
; repeated multiplication. In a similar way, one can perform
; integer multiplication by means of repeated addition. e
; following multiplication procedure (in which it is assumed
; that our language can only add, not multiply) is analogous
; to the expt procedure:
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
; This algorithm takes a number of steps that is linear in b .
; Now suppose we include, together with addition, opera-
; tions double , which doubles an integer, and halve , which
; divides an (even) integer by 2. Using these, design a mul-
; tiplication procedure analogous to fast-expt that uses a
; logarithmic number of steps

; For now, just do positive numbers.
; Initial sum is 0.

; Idea: a*b
; If b is even: a*b = 2a * b/2
; If b is odd: a*b = a + a * b - 1
; if b is 0, return the current sum.

; Idea: keep track of the current product.

(define (double x)
    (+ x x))

(define (halve x)
    (/ x 2))

(define (is-even x)
    (= 0 (remainder x 2)))

(define (fast-mult a b)
    (define (fast-mult-iter a b sum)
        (cond   ((= 0 b) sum)
                ((is-even b) (fast-mult-iter (double a) (halve b) sum ))
                (else (fast-mult-iter a (- b 1) (+ sum a)))))
    (fast-mult-iter a b 0))


 (fast-mult 3 4)
 (fast-mult 5 5)
 (fast-mult 23 24)
 (fast-mult 12 12)
 (fast-mult 3 27)
 (fast-mult 0 23)
 (fast-mult 23 0)