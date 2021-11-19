#lang racket
(require "constraint-propagation.rkt")
; one way to avoid the trouble in 3.34: define squarer as a new
; primitive constraint.

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        ; b having a value is enough to determine a.
        (if (< (get-value b) 0)
            ; the square cannot be negative
            (error "square less than 0: SQUARER"
                   (get-value b))
            ; we can determine the value of a
            (set-value! a (sqrt (get-value b)) me))
        ; B does not have a value. We should check a.
        (when (has-value? a)
            ; A has a value that we can use to get B
            (set-value! b (expt (get-value a) 2) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: SQUARER"
                       request))))
  ; Connect the connectors
  (connect a me)
  (connect b me)
  me)

(module+ test
  (require rackunit)
  (define A (make-connector))
  (define B (make-connector))
  (squarer A B)
  (probe "A value" A)
  (probe "B value" B)
  (set-value! A 4 'user)
  (get-value B)
  (forget-value! A 'user)
  (set-value! B 100 'user)
  )