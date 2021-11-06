#lang racket

;Fermat test idea:
;Comes from Fermat's little theorem.
;If n is a prime number, a is a pos. integer less than n,
;then a^n is congruent to a modulo n.
;
;So the idea:
;If n is not prime, then most of the numbers a < n
;will not satisfy the above relation. There are some
;exceptions though.
;Given n, pick a < n, compute remainder of a^n modulo n.
;If equal to a, chances good that it is prime.
;Pick again.
;First, we need a procedure to compute a^n modulo m.
;The idea:
;Given m, x cong. rem(x, m) modulo m.
;         y cong. rem(y, m) modulo m.
;         so xy cong. rem(x,m) * rem(y,m) modulo m.
;
;So to compute x^m modulo n:
;base case m = 0, return 1.
;m even? compute (rem(x^m/2, m))^2 mod m
;m odd?
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((is-even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (helper a)
    (= a (expmod a n n)))
  (helper (+ 1 (random (- n 1)))))


(require sicp-helpers/mathlib)

(provide fast-prime? smallest-divisor prime?)


; idea: number is prime if smallest divisor is itself.

(define (smallest-divisor x)
  (define (helper cur)
    (cond ((> (square cur) x) x)
          ((= 0 (remainder x cur)) cur)
          (else (helper (+ 1 cur)))))
  (helper 2))


(module+ test 
  (require rackunit)
  (check-true (fast-prime? 2 10))
  (check-true (fast-prime? 3 10))
  (check-true (fast-prime? 5 10))
  (check-false (fast-prime? 20 10))
  (check-true (fast-prime? 1823 10)))