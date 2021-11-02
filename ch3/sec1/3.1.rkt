#lang racket

; Accumulator: procedure called repeatedly with a single numeric
; argument and accumulates its arguments into a sum.
; Each time called, return the currently accumulated sum.

; write procedure make-accumulator that generates accumulators
; with independent sums

; current-sum is the state variable
; return a function that takes a single argument
; that adds to the total and returns the current total
(define (make-accumulator current-sum)
  (lambda (add-to)
    (begin
      (set! current-sum (+ current-sum add-to))
      current-sum)))

(module+ test
  (require rackunit)
  (define A (make-accumulator 5))
  (define B (make-accumulator 5))
  (check-eq? 15 (B 10))
  (check-eq? 25 (B 10))
  (check-eq? 15 (A 10))
  (check-eq? 25 (A 10))
  )