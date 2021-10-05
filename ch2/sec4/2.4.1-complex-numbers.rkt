#lang racket

(require sicp/mathlib)

; Suppose we have selectors for complex numbers:
; real-part, imag-part, magnitude, and angle

; Suppose we also have constructors for
; both rectangular and polar form:
; make-from-real-imag
; make-from-mag-ang

; Then we can add/subtract in terms of real, imag.
; Multiplying in terms of magnitude and angle

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

; But the question is:
; what is the reprentation for complex numbers, and how are selectors
; defined?