#lang racket

(require "1.42.rkt")
(require sicp/mathlib)

; repeated application n times:
; as a recursive procedure:
; if n = 1, then it is just the original function.
; if n > 1, then it is a function taking x
; and applying f to x a total of n times.
; Idea: compose f with (repeated f n-1)
; This is the function that calculates f (f^(n-1)(x))
; Final return value: a function taking a single input x
; and applying f n times to x.
(provide repeated)

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(module+ test
  (require rackunit)
  (check-eq? ((repeated square 2) 5) 625))