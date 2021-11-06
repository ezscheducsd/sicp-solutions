#lang racket

(require sicp-helpers/mathlib)

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

; Mid point of (x1, y1) and (x2, y2):
; (avg(x1, x2), avg(y1, y2))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (make-point
     (average (x-point start) (x-point end))
     (average (y-point start) (y-point end)))))


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(module+ test
  (require rackunit)
  (define p1 (make-point -3 6))
  (define p2 (make-point 7 -4))
  (define seg (make-segment p1 p2))
  (define midpoint (midpoint-segment seg))
  (print-point midpoint))