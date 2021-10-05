#lang racket
; T_pq(a, b) = (bq + aq + ap, bp + aq) = x
; T_pq(x) = 
; (
;     (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p,
;     (bp + aq)p + (bq + aq + ap)q
; )

; = 
; (
;     /bpq + aqq + /bqq + aqq + apq + /bpq + apq + app,
;     /bpp + /apq + /bqq + /aqq + /apq
; )

; = 
; (
;     b(2pq + qq) + a(2pq + qq) + a(pp + qq)
;     b(pp + qq) + a(2pq + qq)
; )

(define (square x)
    (* x x))

(define (fib n)
(fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
(cond ((= count 0) b)
((even? count)
(fib-iter a
b
(+ (square p) (square q))
(+ (* 2 p q) (square q))
(/ count 2)))
(else (fib-iter (+ (* b q) (* a q) (* a p))
(+ (* b p) (* a q))
p
q
(- count 1)))))

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 10)
(fib 13)