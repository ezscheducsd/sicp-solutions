#lang racket

(require sicp-pict)

; below paints the first below the second
; painter1 is below painter2

; painter1 will be in frame (0 0) (1 0) (0 0.5)
; painter2 will be in frame (0 0.5) (1 0.5) (1 1)
(define (below painter1 painter2)
  (let* ((split-point (make-vect 0 0.5))
         (paint-below
          (transform-painter painter1
                             (make-vect 0 0)
                             (make-vect 1.0 0)
                             split-point))
         (paint-above
          (transform-painter painter2
                             split-point
                             (make-vect 1 0.5)
                             (make-vect 1 1))))
    (lambda (frame)
      (paint-below frame)
      (paint-above frame))))

(module+ test
  (define test-frame (make-frame
                      (make-vect 0 0)
                      (make-vect 1 0)
                      (make-vect 0 1)))
  (define my-black (number->painter 0))
  (define my-white (number->painter 255))
  (paint my-black)
  )