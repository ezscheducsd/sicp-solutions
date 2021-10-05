#lang racket

; directed line segment: pair of vectors.
; vector running from origin to start point of segment and
; vector running from origin to end-point of the segment.

; use vectors from 2.46 to define a representation for
; segments with a constructor make-segment and selectors
; start, end

; we can do this as another simple pair.

(require "2.46.rkt")

; start and end are vectors representing the start and end points
; of the segment.
(define (make-segment start end)
  (cons start end))

(define start-segment car)

(define end-segment cdr)