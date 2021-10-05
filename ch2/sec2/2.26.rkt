#lang racket

(define x (list 1 2 3))
(define y (list 4 5 6))

(module+ test
  (append x y)
  ; This should just be the list (1 2 3 4 5 6)
  (cons x y)
  ; This should be the list containing four elements:
  ; (1 2 3), 4, 5, 6
  (list x y)
  ; This should be the list containing two elements:
  ; (1 2 3) and (4 5 6)
  )