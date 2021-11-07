#lang racket

(require sicp-helpers/mathlib)

(define (double f)
  (lambda (x) (f (f x))))

(module+ test
  (((double (double double)) inc) 5)
  ((double inc) 5)
  )

; (double double)
; x -> (double (double x))

; What is (double x)?
; y -> (x (x y))

; Now what is (double (double x))?
; This is a function taking y
; that applies x to y four times.

; so (double double) is a function
; that takes a function f
; and return a function that, given x,
; applies f to x four times.

; Now what is (double (double double))?
; This is a function that given x,
; applies (double double) to x twice.
; What happens after the first application?

; So x would be a function.
; After the first application,
; we get a function Y: y -> x^4(y)

; What happens after the second application?
; basically ((double double) Y)
; this is a function that, given z, applies
; Y to z four times.
; So we apply x to z 16 times in total.

; So (double (double double))
; is a function that given x,
; returns a function taking z that applies
; x to z 16 times.

; ((double (double double)) inc)
; is a function taking z, applies inc to z 16 times.