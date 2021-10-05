#lang racket

(require "2.2.3-sequences.rkt")
(require sicp/mathlib)

; unique-pairs
; given integer n, generate the sequence of pairs
; (i, j) with 1 <= j < i <= n
; first, generate i = 1...n
; For each i, get a list of pairs (i, j), where j < i.
; Then combine these in a flat map
(provide unique-pairs)
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j)
            (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

; Now we want to filter out pairs that have a prime sum.
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

; Finally, we want to generate a triple from each pair,
; where the third item is the sum of the previous two.
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
            (unique-pairs n))))

(module+ test
  (prime-sum-pairs 5))