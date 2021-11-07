#lang racket

(provide make-interval
         upper-bound
         lower-bound
         add-interval
         sub-interval
         mul-interval
         div-interval
         display-interval)

(define (display-interval i)
  (newline)
  (display "[")
  (display (lower-bound i))
  (display ", ")
  (display (upper-bound i))
  (display "]"))

; Exercise 2.7: make-interval, selectors
(define (make-interval a b) (cons a b))

(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

; Adding intervals: simply add the lower bounds and the upper bounds.
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; Exercise 2.8: Subtracting intervals
; Subtracting intervals i1 - i2: the lowest bound is
; the lower bound of i1 - the upper bound of i2.
; The upper bound is the upper bound of i1 - lower bound of i2.
(define (sub-interval x y)
  (make-interval
   (- (lower-bound x) (upper-bound y))
   (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; Exercise 2.10: dividing by an interval that spans 0.
(define (div-interval x y)
  (define (spans-0? i)
    (let ((lower (lower-bound i))
          (upper (upper-bound i)))
      (and (<= lower 0) (>= upper 0))))
  (if (spans-0? y)
      (error "Cannot divide by an interval that spans 0.")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

(module+ test)