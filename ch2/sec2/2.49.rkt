#lang racket

(require sicp-pict)

(define ms make-segment)
(define mv make-vect)
(define wave-segments
  (list (ms (mv .4 0) (mv .5 .3))
        (ms (mv .6 0) (mv .5 .3))
        (ms (mv .3 0) (mv .35 .5))
        (ms (mv .7 0) (mv .65 .5))
        (ms (mv .65 .5) (mv 1 .3))
        (ms (mv .25 .45) (mv .35 .5))
        (ms (mv .25 .45) (mv 0 .6))
        (ms (mv .6 .65) (mv .65 .7))
        (ms (mv .4 .65) (mv .35 .7))
        (ms (mv .65 .7) (mv 1 .4))
        (ms (mv .35 .7) (mv .25 .6))
        (ms (mv .25 .6) (mv 0 .73))
        (ms (mv .4 .65) (mv .3 .85))
        (ms (mv .6 .65) (mv .7 .85))
        (ms (mv .3 .85) (mv .5 1))
        (ms (mv .7 .85) (mv .5 1))
        ))

(provide wave-segments)

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


(define wave
  (segments->painter
   (list (ms (mv .4 0) (mv .5 .3))
         (ms (mv .6 0) (mv .5 .3))
         (ms (mv .3 0) (mv .35 .5))
         (ms (mv .7 0) (mv .65 .5))
         (ms (mv .65 .5) (mv 1 .3))
         (ms (mv .25 .45) (mv .35 .5))
         (ms (mv .25 .45) (mv 0 .6))
         (ms (mv .6 .65) (mv .65 .7))
         (ms (mv .4 .65) (mv .35 .7))
         (ms (mv .65 .7) (mv 1 .4))
         (ms (mv .35 .7) (mv .25 .6))
         (ms (mv .25 .6) (mv 0 .73))
         (ms (mv .4 .65) (mv .3 .85))
         (ms (mv .6 .65) (mv .7 .85))
         (ms (mv .3 .85) (mv .5 1))
         (ms (mv .7 .85) (mv .5 1))
         )))

(module+ test
  (paint wave))