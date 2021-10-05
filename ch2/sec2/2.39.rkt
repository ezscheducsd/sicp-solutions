#lang racket

(require "2.38.rkt")

; to reverse a sequence
; append the reverse of the latter list
; to the list containing the current element.
(define (reverse-fold-right sequence)
  (fold-right
   (lambda (x y)
     (append y
             (list x)))
   null sequence))

(define (reverse-fold-left sequence)
  (fold-left
   (lambda (x y)
     (cons y x))
   null sequence))

(module+ test
  (reverse-fold-right (list 1 2 3 4 5))
  (reverse-fold-left (list 1 2 3 4 5))
  )