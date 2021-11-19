#lang racket

(require "constraint-propagation.rkt")

; Squarer attempt.
; Constraint device with two terminals such that the
; value of connector b on the second terminal
; will always be the square of the value a on the first terminal.
; Propose the following:

(define (squarer a b)
  (multiplier a a b))

; What is wrong with this?

; Recall how the multiplier constraint works
; We will need at least two set values to determine the third.

; If we start out by setting b to 25. This is enough information
; to know that a should be 5. But the multiplier doesn't know this.
(define A (make-connector))
(define B (make-connector))
(squarer A B)
(set-value! B '25 'user)
(has-value? A)
; > #f - this is false, even though we should know the value A already.

; So basically we will need some kind of new constraint structure
; for the square.