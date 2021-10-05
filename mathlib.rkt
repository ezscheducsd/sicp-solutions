#lang racket

(provide square is-even? cube inc)
(provide fast-expt remainder gcd average)
(provide prime?)

(define (square x)
  (* x x))

(define (is-even? x)
  (= 0 (remainder x 2)))

(define (cube x)
  (* x x x))

(define (inc x)
  (+ x 1))

(define (identity x)
  x)

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (average x y)
  (/ (+ x y) 2.0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor x)
  (define (helper cur)
    (cond ((> (square cur) x) x)
          ((= 0 (remainder x cur)) cur)
          (else (helper (+ 1 cur)))))
  (helper 2))