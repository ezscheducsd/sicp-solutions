#lang racket

(require sicp/mathlib)

(define (f g) (g 2))

(module+ test
  (f square)
  (f (lambda (z) (* z (+ z 1))))
  (f f)
  )

;What about (f f) ?
; (f f)
; (f 2)
; (2 2)
; This should be an error because we can't apply a number
; as a function.

;(f (lambda (z) (* z (+ z 1))))
;lambdaz: z -> z*(z+1)
;(f lambdaz)
;(lambdaz 2)
;(2*(2+1))
;6