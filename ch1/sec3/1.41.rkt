#lang racket

(require sicp-helpers/mathlib)

(define (double f)
  (lambda (x) (f (f x))))

(module+ test
  (((double (double double)) inc) 5))

; (double double)
; x -> (double (double x))
; (double x) - applies f twice
; (double (double x)) - applies doubling twice - apply an argument
; four times.

; (double (double double)) - applies quadruple twice
; apply a function 8 times.

; ((double (double double)) inc)
; 