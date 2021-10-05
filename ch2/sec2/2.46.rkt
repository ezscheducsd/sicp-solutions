#lang racket

; easy vector operations.
; a vector can just be represented as a pair.
(provide make-vect
         xcor-vect
         ycor-vect
         add-vect
         sub-vect
         scale-vect
         vect-eq?)

(define (make-vect x y)
  (cons x y))

(define xcor-vect car)

(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect c v)
  (make-vect (* c (xcor-vect v))
             (* c (ycor-vect v))))

(define (vect-eq? v1 v2)
  (and (= (xcor-vect v1) (xcor-vect v2))
       (= (ycor-vect v1) (ycor-vect v2))))

(module+ test
  (define v1 (make-vect 2 4))
  (define v2 (make-vect 7 2))
  (require rackunit)
  (define v1+v2 (add-vect v1 v2))
  (check-eq? (xcor-vect v1+v2) 9)
  (check-eq? (ycor-vect v1+v2) 6)
  (define v2-v1 (sub-vect v2 v1))
  (check-eq? (xcor-vect v2-v1) 5)
  (check-eq? (ycor-vect v2-v1) (- 2))
  (define doublev1 (scale-vect 2 v1))
  (check-eq? (xcor-vect doublev1) 4)
  (check-eq? (ycor-vect doublev1) 8)
  )