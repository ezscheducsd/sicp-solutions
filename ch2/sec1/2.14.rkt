#lang racket

(require "2.1.4-interval.rkt")
(require "2.12.rkt")
(require "2.13.rkt")

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

; So it seems like there is more error introduced with the
; first procedure as opposed to the second procedure.
; But why?
; From our experiments, it seems like division adds the tolerances.
; Same for multiplication.
; Addition and subtraction seem to get some value in between.
; Taking the reciprocal seems to preserve tolerance.

; So by this reasoning, it makes sense that par2 is a better procedure.
; It only introduces the tolerances of each term once, as opposed to
; par1. Because par1 introduces the tolerances of each term more than
; once, it makes sense that there will be a greater tolerance
; in the final answer.
(module+ test
  (define A (make-center-percent 100 0.2))
  (define B (make-center-percent 50 0.07))
  (define one (make-center-percent 1 0))
  (define A/A (div-interval A (div-interval A A))) ; 0.02999...
  (define B/B/B (div-interval (div-interval B B) B))
  (display (interval-tolerance B/B/B)) ;0.089
  (define B*B*B (mul-interval (mul-interval B B) B)) ; 0.89
  (newline)
  (display (interval-tolerance B*B*B))
  (define A+B (add-interval A B))
  (newline)
  (display (interval-tolerance A+B))
  (define 1/A (div-interval one A))
  (newline)
  (display (interval-tolerance 1/A))
  )