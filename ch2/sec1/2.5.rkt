#lang racket

; represent pairs of nonneg. integers using only
; numbers and arithmetic operations.

; represent the pair a b as the integer 2^a * 3^b = p
; say the product is p.
; to extract a: divide the product by 3 until no longer divisible by 3.
; then take lg(2^a)

; same idea to get b.

(define (my-cons a b)
  (* (expt 2 a) (expt 3 b)))

; When we extract the 2^a part,
; return the number of times we have to divide by 2 to get 0.
; (lg 2^a)
(define (my-car z)
  (define (get-2a prod)
    (if (= 0 (remainder prod 3))
        (get-2a (/ prod 3))
        prod))
  (define (log2 x)
    (if (= x 1)
        0
        (+ 1 (log2 (/ x 2)))))
  (log2 (get-2a z)))

(define (my-cdr z)
  (define (get-3b prod)
    (if (= 0 (remainder prod 2))
        (get-3b (/ prod 2))
        prod))
  (define (log3 x)
    (if (= x 1)
        0
        (+ 1 (log3 (/ x 3)))))
  (log3 (get-3b z)))

(module+ test
  (require rackunit)
  (define a0b0 (my-cons 0 0))
  (check-eq? (my-car a0b0) 0)
  (check-eq? (my-cdr a0b0) 0)

  (define a0b5 (my-cons 0 5))
  (check-eq? (my-car a0b5) 0)
  (check-eq? (my-cdr a0b5) 5)


  (define a5b0 (my-cons 5 0))
  (check-eq? (my-car a5b0) 5)
  (check-eq? (my-cdr a5b0) 0)

  (define a5b3 (my-cons 5 3))
  (check-eq? (my-car a5b3) 5)
  (check-eq? (my-cdr a5b3) 3))