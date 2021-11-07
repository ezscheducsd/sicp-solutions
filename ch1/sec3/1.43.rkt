#lang racket

; Write a procedure that takes
; a numerical function f, returns
; the procedure that computes f^n(x)

; If n = 1, then this is just the same function.
; If n is greater than 1, then
; we can compose f with the repeated (n - 1) version.

(require "1.42.rkt")
(require sicp-helpers/mathlib)

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(provide repeated)

(module+ test
  ((repeated square 2) 5)
  )