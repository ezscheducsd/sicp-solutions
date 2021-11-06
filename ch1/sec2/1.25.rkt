#lang racket

;Exercise 1.25: Alyssa P. Hacker complains that we went to
;a lot of extra work in writing expmod . After all, she says,
;since we already know how to compute exponentials, we
;could have simply written
;
;(define (expmod base exp m)
;  (remainder (fast-expt base exp) m))

; Would this procedure serve as well for our fast prime tester?

; remember, expmod runs in log(n) time, where n is the number
; being tested for prime.
; fast-expt would also run in log(n) time, where n is the number
; being tested for prime.

; However, we cannot say that this would be better because for
; larger n, we need to perform computations with huge numbers.
; This might not be viable in some computing environments
; and can certainly lead to overflow.