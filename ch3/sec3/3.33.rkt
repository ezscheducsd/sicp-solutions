#lang racket

(require "constraint-propagation.rkt")

; Define an averager connector that takes
; 3 connectors a, b, c as inputs and establishes
; the constraint that c is the average of a and b.

; Relation is c = (a + b) / 2
; c = 1/2 * (a + b)
; So we will have a top level multipler constraint
; involving connectors c, constant 1/2, and a sum

; The Sum constraint will involve
; connector d, connector a, and connector b

(define (average-of-two a b c)
  (let ((one-half (make-connector))
        (s (make-connector)))
    (multiplier one-half s c)
    (adder a b s)
    (constant 0.5 one-half)
    'ok))

(module+ test
  (require rackunit)
  (define A (make-connector))
  (define B (make-connector))
  (define C (make-connector))
  (average-of-two A B C)
  (probe "A value" A)
  (probe "B value" B)
  (probe "C value" C)

  (set-value! A 10 'user)
  (set-value! B 20 'user)
  (forget-value! A 'user)
  (set-value! C 10 'user)
  )