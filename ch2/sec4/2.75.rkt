#lang racket

(require sicp/mathlib)

; Smart object make-from-mag-ang
; dispatches on operation symbols.
(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* mag (cos ang)))
          ((eq? op 'imag-part) (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

; Note that this generic operation can only have one argument.
; Since we are letting only ONE object dispatch on the operation
; type 
(define (apply-generic op arg) (arg op))

(module+ test
  (define c (make-from-mag-ang 1 (/ pi 4)))
  (c 'real-part)
  (c 'imag-part)
  (c 'magnitude)
  (c 'angle)
  )