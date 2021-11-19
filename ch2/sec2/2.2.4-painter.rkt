#lang racket
(require sicp-pict)

; This module implements frames/painters
; at least to the lowest level we can

; frame coordinate map: map
; unit square points to the frame
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

; Exercise 2.46: Vector representation
(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))
(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))
; Scale vector v by c
(define (scale-vect c v)
  (make-vect
   (* c (xcor-vect v))
   (* c (ycor-vect v))))


; Exercise 2.47: Constructors and selectors for frames
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
; Frame selectors are origin, edge1 and edge2
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

; Exercise 2.48: directed line segments
; represented as a pair of vectors.
; origin to start of seg
; origin to end of seg
(define (make-segment v1 v2)
  (cons v1 v2))
(define start-segment car)
(define end-segment cdr)

; Exercise 2.49:
; I'll skip this one for now. It is just a matter
; of choosing the correct line segments to draw the
; desired picture.

; Painters.
; These are procedures that, given a frame as an argument,
; draws a particular image shifted and scaled to fit the frame

; Painter operations are based on the procedure
; transform-painter, which takes as args a painter and information
; on how to transform a frame and produce a new painter
; Painter: the original painter to transform
; origin: new frame origin
; corner1, corner2: specify the ends of the edge vectors.
; not relative to the new origin
; The arguments to transform-painter are within
; the unit square

; So the idea is that we want to transform the old frame.
; We will map the new origin within the current
; frame map
; each corner is the difference b/t mapped new corner and
; the new origin

; flip vertically
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 1 1)
                     (make-vect 0 0)))

; Shrink to the upper right
; of the frame, centered at the center.
(define (shrink-to-upper-right painter)
  (transform-painter
   painter (make-vect 0.5 0.5)
   (make-vect 1.0 0.5) (make-vect 0.5 1.0)))

; Beside takes two painters, transforms them to paint
; in the left and right halves of an argument frame
; respectively
; Produce a new compound painter
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            split-point
            (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))