#lang racket

(require "2.2.3-sequences.rkt")
(require "2.40.rkt")

; Find all ordered triples of distinct
; positive integers i, j, k less than
; or equal to n that sum to s.

; Can we use unique pairs as a sub-procedure?
; Idea: generate all i = 1...n first.
; Then, for each i we will want a list
; of triples (i, j ,k) such that 1 <= k < j < i <= n.
; For each i, we can select all the unique pairs up
; to i - 1.
; Then we append i to the beginning of the pair

(define (unique-triples-2 n)
  (flatmap (lambda (i)
             (map (lambda (unique-pair)
                    (cons i unique-pair))
                  (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))

; Ordered triples that sum to a given integer s.


; Report the triples and their sum
(define (make-triple-with-sum triple)
  (append triple
          (list (+ (car triple) (cadr triple) (cadr (cdr triple))))))

; First, enumerate all ordered triples bounded by n.
; Then filter out by sum equal to s.
; Then convert each triple to a quadruple with the last
; element being a sum.
(define (triples-sum-s n s)
  (define (sum-is-s? triple)
    (= s (+ (car triple) (cadr triple) (cadr (cdr triple)))))
  (map make-triple-with-sum
       (filter sum-is-s?
               (unique-triples-2 n))))

(module+ test
  (triples-sum-s 7 12))