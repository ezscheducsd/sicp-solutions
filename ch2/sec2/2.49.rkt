#lang racket

(require "2.2.4-painter.rkt")
(require "2.46.rkt")
(require "2.48.rkt")

; the painter that draws the outine of the designated frame.
; We just need (0,0) -> (0, 1), (0, 1) -> (1, 1), etc.

; points in the unit square from 0 -> 1:
(define unit-square-bl
  (make-vect 0 0))

(define unit-square-br
  (make-vect 1 0))

(define unit-square-tl
  (make-vect 0 1))

(define unit-square-tr
  (make-vect 1 1))

; painter that draws the outine of the designated frame.
(define outline-painter
  (segments->painter
   (list (make-segment unit-square-bl unit-square-br)
         (make-segment unit-square-br unit-square-tr)
         (make-segment unit-square-tr unit-square-tl)
         (make-segment unit-square-tl unit-square-bl))))


; painter that draws an "X" by connecting opposite corners of
; frame
(define x-painter
  (segments->painter
   (list (make-segment unit-square-bl unit-square-tr)
         (make-segment unit-square-br unit-square-tl))))


; connect midpoints of the sides of the frame to make a diamond shape.
; for example, the bottom left segment will run from (0, 0.5) -> (0.5, 0)
(define diamond-painter
  (segments->painter
   ))