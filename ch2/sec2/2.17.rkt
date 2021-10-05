#lang racket

(define (last-pair l)
  (cond ((null? l) (error "last-pair cannot be applied to an empty list."))
        ((null? (cdr l)) l)
        (else (last-pair (cdr l)))))

(module+ test
  (define l1 (cons 10 null))
  (define l2 (list 1 2 3 4))
  (display (last-pair l1))
  (display (last-pair l2))
  )