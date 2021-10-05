#lang racket

(require "2.2.3-sequences.rkt")
(provide fold-right fold-left)

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; What should the property of op be to
; guarantee that that result of fold left
; and fold right produce the same result?

; for op: we need that (assuming initial)
; and assuming a list of (x y z)
; x op (y op (z op initial)) =
; ((initial op x) op y) op z

; The operation should be: associative and commutative.

(module+ test
  (fold-right / 1 (list 1 2 3))
  ; 1 / (2 / 3)
  ; should be 3/2 = 1.50
  (fold-left / 1 (list 1 2 3))
  ; ((1/1) / 2) / 3
  ; should be 0.1666 1/6.
  (fold-right list null (list 1 2 3))
  ; (list 1 (list 2 (list 3 nil)))
  ; should be (1 (2 (3 ())))
  (fold-left list null (list 1 2 3))
  ;(list (list (list null 1) 2) 3)
  ; should be ( ( (() 1) 2 ) 3 )
  )