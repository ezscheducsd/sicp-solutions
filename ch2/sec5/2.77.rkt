#lang racket

(require "../sec4/2.4.3-complex-data-directed.rkt")
(require "2.5.1-generic-arithmetic.rkt")

; See the 2.77 changes in 2.5.1
; ASSUming the changes in 2.77 were made.

; This works. Why?
; Recall how apply-generic works.
; It first creates a list of types for its arguments
; then, with the given symbolic operation
; looks up the appropriate procedure
; and applies that to the CONTENTS of each argument.

; so c below is ('complex ('rectangular pair(1, 1)))

; When we call magnitude on c,
; this evaluates to
; (magnitude c)
; ((get 'magnitude ('complex)) ('rectangular pair(1,1)))
; Looking up get 'magnitude 'complex
; returns the same generic magnitude procedure
; that uses apply-generic.
; So we call magnitude again.
; (magnitude ('rectangular pair(1,1)))
; ((get 'magnitude ('rectangular)) pair(1,1))
; get 'magnitude ('rectangular) is the
; magnitue procedure that the rectangular package installed
; in 2.4.3. This works now.

; So what we had to do was account for the multi-levels of tagging.

; apply-generic is applied twice in this case.

(module+ test
  (define c (make-complex-from-real-imag 1 1))
  c
  (magnitude c)
  (real-part c)
  )