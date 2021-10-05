#lang racket


;Exercise 2.84: Using the raise operation of Exercise 2.83,
;modify the apply-generic procedure so that it coerces its
;arguments to have the same type by the method of succes-
;sive raising, as discussed in this section. You will need to
;devise a way to test which of two types is higher in the
;tower. Do this in a manner that is “compatible” with the
;rest of the system and will not lead to problems in adding
;new levels to the tower.

; We always assume a tower of types in this scenario.

; Idea:
; We will need to find out which type is higher in the tower.
; How?
; Given two types t1 and t2:
; Arbitrarily choose t1.
; If there is no raise procedure, then t1 has to be the higher type.
; If we can raise t1 to t2, then t2 has the higher type.
; Otherwise, raise t1 to t1', then try again for t1' and t2.

(require "2.5.2-generic-arithmetic-raise.rkt")

(module+ test
  (define c (make-complex-from-real-imag 1 1))
  (define r (make-rational 1 2))
  (define s (make-scheme-number 1))

  (add c r)
  (add r s)
  (add s r)
  (add r c)
  (add c s)
  (add s c)
  )