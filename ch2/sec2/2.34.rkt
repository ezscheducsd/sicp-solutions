#lang racket

(require "2.2.3-sequences.rkt")

; Horner's rule:
; Given coefficients a0...an:
; Start with an, multiply by x, add an-1, multiply by x.
; until we add a0. (no multiplying by x after a0.)

; Accumulate: add the current term to x times the rest
; of the horner sum.
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)) )
              0
              coefficient-sequence))

; Coefficients: (4 3 2 1)
; x = 2
; 1(2)^3 + 2(2)^2 + 3(2)^1 + 4
; = 26
(module+ test
  (require rackunit)
  (check-eq? (horner-eval 2 (list 4 3 2 1)) 26)
  )