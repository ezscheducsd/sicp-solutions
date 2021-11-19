#lang racket
(require "constraint-propagation.rkt")

; c+, c*, etc. are the "constraint" versions of the arithmetic
; operations. c+ takes two connectors as arguments
; and returns a connector that is related to these by an adder
; constraint
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

; Can we define c- and c/ in terms of adder and multiplier?
; duh a - b = c is a = c + b
; a / b = c is a = c * b
(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (cv v)
  (let ((c (make-connector)))
    (constant v c)
    c))

(module+ test
  ; celsius-fahrenheit-converter is
  ; cumbersome when compared with a more expression-oriented style
  ; of definition, such as:
  (define (celsius-fahrenheit-converter x)
    (c+ (c* (c/ (cv 9.0) (cv 5.0))
            x)
        (cv 32.0)))
  (define C (make-connector))
  (define F (celsius-fahrenheit-converter C))
  (probe "C value" C)
  (probe "F value" F)
  (set-value! C 20 'user)
  (forget-value! C 'user)
  (set-value! F 85 'user)
  )