#lang racket

;Average damping:
;Given f and x,
;when the new procedure is applied to x,
;return the average of x and f(x)

(require "1.3.3-fixed-point.rkt")
(require sicp/mathlib)

(provide average-damp)

(define (average-damp f)
  (lambda (x) (average x (f x))))

;Remember: square root of x is finding
;y such that y^2 = x, y = x/y.
;Using average damping, y + y = x/y + y.
;2y = x/y + y
;y = (x/y + y)/2
;So, find a fix point of y = (x/y + y)/2
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(module+ test
  (sqrt 24))