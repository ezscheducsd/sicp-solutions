#lang racket

; monte carlo integration
; estimate definite integrals by means of a monte-carlo
; simulation.

; Consider computing the area f a region of a space
; described by predicate P(x, y) that is true for points
; in the region and false for points outside the region.

; To estimate the region described by such a predicate,
; begin by chooseing a rectangle the contains the region.
; Desired integral is the area of the portion of the rectangle
; that lies in the region.

; Estimate by picking, at random, points (x, y) in the rectangle
; then test using predicate P.
; If we try with many points, fraction of points that fall in the
; region should give an estimate of the proportion of the rectangle
; that lies in the region
; multiply this fraction by the area of the region.

; Note: it is essential that the rectangle contains the region
; because if not, then the area returned will just be the
; same as the rectangle

; x1 and x2: x bounds of rectangle
; y1 and y2: y bounds of rectangle
; P: given x, and y, return if x, y is within the desired area.

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

; NOTE: we use the same program structure as the monte-carlo
; pi estimation from 3.1.2
(define (estimate-integral P x1 x2 y1 y2 trials)
  ; Generate random b/t 0 and 1,
  ; multiply by range
  ; add to low
  (define (randx)
    (let ((x-range (abs (- x2 x1))))
      (+ x1 (* (random) x-range))))
  (define (randy)
    (let ((y-range (abs (- y2 y1))))
      (+ y1 (* (random) y-range))))
  (define (in-area-test)
    (let ((x (randx))
          (y (randy)))
      (P x y)))
  (define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
      (cond ((= trials-remaining 0)
             (/ trials-passed trials))
            ((experiment)
             (iter (- trials-remaining 1)
                   (+ trials-passed 1)))
            (else
             (iter (- trials-remaining 1)
                   trials-passed))))
    (iter trials 0))
  (let ((area-rect (* (abs (- x2 x1)) (abs (- y2 y1))))
        (frac (monte-carlo trials in-area-test)))
    (* frac area-rect)))


(module+ test
  ; Unit circle predicate
  (define (P x y)
    (<= (+ (expt x 2) (expt y 2)) 1))
  (estimate-integral P -2 2 -2 2 100000)
  )