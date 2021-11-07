#lang racket

(require sicp-helpers/mathlib)

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (same-point? p1 p2)
  (and (= (x-point p1) (x-point p2))
       (= (y-point p1) (y-point p2))))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (length-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (sqrt (+ (square (- (x-point end) (x-point start)))
             (square (- (y-point end) (y-point start)))))))

(define (share-endpoint? s1 s2)
  (let ((start-s1 (start-segment s1))
        (start-s2 (start-segment s2))
        (end-s1 (end-segment s1))
        (end-s2 (end-segment s2)))
    (or (same-point? start-s1 start-s2)
        (same-point? start-s1 end-s2)
        (same-point? end-s1 start-s2)
        (same-point? end-s1 end-s2))))

; Remember, slope is y2 - y1 / x2 - x1
(define (slope segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (/ (- (y-point end) (y-point start))
       (- (x-point end) (x-point start)))))

; Mid point of (x1, y1) and (x2, y2):
; (avg(x1, x2), avg(y1, y2))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (make-point
     (average (x-point start) (x-point end))
     (average (y-point start) (y-point end)))))

; Two segments are perpendicular if the
; slope of one segment is the
; negative reciprocal of the other
(define (perpendicular? s1 s2)
  (= (slope s1)
     (/ -1 (slope s2))))

(provide make-segment
         make-point
         x-point
         y-point
         start-segment
         end-segment
         length-segment
         perpendicular?
         midpoint-segment
         share-endpoint?
         )


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(module+ test
  (require rackunit)
  (define p1 (make-point 1 -2))
  (define p2 (make-point -3 1))
  (define seg (make-segment p1 p2))
  (define midpoint (midpoint-segment seg))
  (slope seg)

  ; Perpendicular test
  (define p3 (make-point -2 -2))
  (define p4 (make-point 2 2))
  (define seg1 (make-segment p3 p4))
  (define p5 (make-point -2 2))
  (define p6 (make-point 2 -2))
  (length-segment seg1)
  (define seg2 (make-segment p5 p6))
  (perpendicular? seg1 seg2)
  (share-endpoint? seg1 seg2)
  (print-point midpoint))