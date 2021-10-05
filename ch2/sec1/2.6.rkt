#lang racket

; Zero is a function taking in a single parameter
; f and returning the identity function.
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; evaluating (add-1 zero)
; (add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x)))))
; (lambda (f) (lambda (x) (f ((zero f) x)))))
;(lambda (f) (lambda (x) (f ((lambda (x) x) x)))))
;(lambda (f) (lambda (x) (f x))))

; So one is a function taking in a single parameter
; f.
; It returns a function taking in a single parameter x
; that applies f to x once.

; So two should be the function that takes a single parameter
; f and returns a function taking in x and applying f to x twice.
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

; Addition procedure; (not in terms of add-1.)
; When adding, the result should be
; a function with param f, returning a function with param x
; that applies f to x the sum amount of times.

; first, apply x to (a1 f)
; then apply the result of this to (a2 f)
(define (add a1 a2)
  (lambda (f) (lambda (x) ((a2 f) ((a1 f) x)))))