#lang racket

(require "2.2.3-sequences.rkt")
(require sicp-helpers/mathlib)

; representing some  list-manipulation operations
; as accumulations:
; map in terms of accumulate.
; remember:
; the operation given to accumulate operates on the
; current list item and the accumulation of the rest
; of the list.
; So the operation should apply the procedure to x
; and cons it with the accumulation of the rest of
; the list.
(define (map-accumulate p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y)) null sequence))

; append in terms of accumulate.
; Final result: a list containing all elements
; of seq1 and seq2.
; initial result should be seq2.
; Then we will cons every item of seq1.
(define (append-accumulate seq1 seq2)
  (accumulate cons
              seq2 seq1))

; length in terms of accumulate.
; For each element, we don't have to apply any procedure.
; We just have to note its existence by adding
; 1 to the total length.
(define (length-accumulate sequence)
  (accumulate
   (lambda (x y) (+ 1 y)) 0 sequence))

(module+ test
  (define l (list 1 2 3 4))
  (map-accumulate square l)
  (append-accumulate (list 1 2 3 4) (list 5 6 7 8))
  (length-accumulate (list 1 2 3 4 5 6))
  )