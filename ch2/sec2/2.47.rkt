#lang racket

; for a frame, we need to be able to select the origin vector,
; edge1, and edge2
(require "2.46.rkt")

(provide make-frame
         origin-frame
         edge1-frame
         edge2-frame)


(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cadr (cdr frame)))


(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-2 frame)
  (car frame))

(define (edge1-frame-2 frame)
  (cadr frame))

(define (edge2-frame-2 frame)
  (cdr (cdr frame)))


(module+ test
  (require rackunit)
  (define f1o (make-vect 0 0))
  (define f1e1 (make-vect 1 2))
  (define f1e2 (make-vect 3 0))
  (define f1 (make-frame f1o f1e1 f1e2))
  (define f2o (make-vect 1 1))
  (define f2e1 (make-vect 5 1))
  (define f2e2 (make-vect 1 7))
  (define f2 (make-frame-2 f2o f2e1 f2e2))

  (check-eq? (origin-frame f1) f1o)
  (check-eq? (edge1-frame f1) f1e1)
  (check-eq? (edge2-frame f1) f1e2)

  (check-eq? (origin-frame-2 f2) f2o)
  (check-eq? (edge1-frame-2 f2) f2e1)
  (check-eq? (edge2-frame-2 f2) f2e2)
  )