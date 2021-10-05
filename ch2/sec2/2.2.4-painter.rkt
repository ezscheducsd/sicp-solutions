#lang racket

(require "2.46.rkt")

; A painter library for displaying pictures
; and combining painters.

; beside operation on two painters - takes
; two painters and produces a new, compound painter that
; draws the first image in left half and second image
; in right half.

; below: draw first image below second image.

; flip-vert - draw painter's image upside down.

; flip-horiz - draw painter's image left-right reversed.

; draw a painter's image beside its flipped image.
; (define wave2 (beside wave (flip-vert wave)))

; draw wave2 below wave2.
;(define wave4 (below wave2 wave2))

; draw a painter's image alongside its flipped image.
; Then repeat twice on bottom and top.
;(define (flipped-pairs painter)
;  (let ((painter2 (beside painer (flip-vert painter))))
;    (below painter2 painter2)))

; an instance of the flipped-pairs pattern:
; (define wave4 (flipped-pairs wave))

; recursive operations.

; right-split:
; get a smaller image of right-split n-1.
; Then draw original image beside
; the smaller below the smaller
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

; balanced patterns - branching upwards as well as toward the right.
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

; place four copies of corner split appropriately
; square-limit.

;(define (square-limit painter n)
;  (let ((corner (corner-split painter (- n 1))))
;    (let ((top (beside (flip-horiz corner) corner)))
;      (below (flip-vert top)
;             top))))

; Abstracting patterns operations:
; square-limit and flipped-pairs
; both arrange four copies of a painter's image
; in a square pattern
; Only difference: orientation of copies.

; Square of four takes four transformations:
; transformations to apply to the top left,
; top right, bottom left, and bottom right respectively.
; We return a function that takes a painter
; and transforms the painter to create a four-square
; using the respective transformations.
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))


; flipped pairs:
; transformation to apply to top left: identity
; top right: flip vertically
; bottom left: identity
; bottom right: flip vertically
(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

; square limit:
; top right: corner split n-1
; top left: flip horiz, corner split n-1
; bottom left: flip vert, flip horiz, corner split n-1
; bottom right: flip vert, corner split n-1
; We could apply corner split to all copies of the
; painter first
; then apply the horiz/vertical transformations.
(define (square-limit painter n)
  (let ((corner (corner-split painter (- n 1))))
    (let ((combine4 (square-of-four flip-horiz
                                    identity
                                    (compose flip-vert flip-horiz)
                                    flip-vert)))
      (combine4 corner))))

; Frames - origin vector (relative to some absolute plane origin)
; edge1, edge2.
; Use coords in unit square (0 <= x, y <= 1) to specify IMAGES.
; Produce painter p’s image in f by calling p with f as an argument.e map - shift and scale images to fit the frame.
; transform unit square into the frame by mapping V = (x, y)
; to origin(frame) + x*edge1(frame) + y*edge2(frame)

; coordinate map procedure
; given a frame, return a procedure that maps a vector to another
; vector.
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

(require "2.23.rkt")


; Painter: procedure that, give a frame as an argument,
; draws a particular image shifted and scaled to fit the frame.
; Suppose we have a proc. draw-line that draws a line on the screen
; b/t two specified points.
; painters for line drawings:
; given a list of line segments,
; draw a line for each frame-mapped
; line segment.
(provide segments->painter)

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))


; Transforming painters:
; create a painter that invokes original painters with
; respect to frames derived from the argument frame
(require "2.47.rkt")
; transform painter - take a painter and arguments on how
; to transform a frame
; origin, corner1, and corner2 should be units in the
; unit square.
; We will map these using the given frame to create a new
; frame in which to draw relative to old frame.
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

; flip painter vertically:
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

; A painter that shrinks its image to the upper-right quarter
; of the frame given:
(define (upper-right-quarter painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1 0.5)
                     (make-vect 0.5 1)))

; rotate counterclockwise 90 degrees
; think about rotating the whole frame 90 degrees.
; where should the origin, edge1, and edge2 point to
; in terms of a unit square?
(define (rotate-cc-90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

; squash images toward the center of the frame:

; beside painter
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

; Exercise 2.50: flip-horiz.
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; Exercise 2.50: rotate counter clockwise 180
(define (rotate-cc-180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

; Exercise 2.50: rotate 270 degrees
(define (rotate-cc-270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; Exercise 2.51: Below
; We can implement it similar to beside
; The split point in this case is 0.0, 0.5
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            split-point))
          (paint-above
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.5)
            (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-below frame)
        (paint-above frame)))))


; We can also implement below in terms of beside
; and operations from 2.50.
; Rotate both bottom and top 270 deg. counter clockwise.
; Then place bottom beside top
; Then rotate the entire thing 90 degrees
(define (below2 painter1 painter2)
  (let ((below-rot-270
         (rotate-cc-270 painter1))
        (above-rot-270
         (rotate-cc-270 painter2)))
    (let ((beside-below-above
           (beside below-rot-270 above-rot-270)))
      (rotate-cc-90 beside-below-above))))