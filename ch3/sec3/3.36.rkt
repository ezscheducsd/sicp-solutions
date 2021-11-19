#lang racket
(require "constraint-propagation.rkt")

; Suppose we evaluate the following in the global environment
(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)

; At some point during the evaluation of the set-value!, the
; expression from the connector's local procedure is evaluated
; (for-each-except setter inform-about-value-constraints)

; I'll do this sometime later. 