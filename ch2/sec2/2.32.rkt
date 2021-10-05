#lang racket

; The subsets of the null set is
; just the null set.
(define (subsets s)
  (if (null? s)
      (list null)
      ; Idea to compute the subsets:
      ; Subsets will either contain the
      ; first item s1 or they will not.
      ; So first, we need all the subsets
      ; of the set not containing s1. (subsets (cdr s))
      ; Then, for each of those subsets, we want to
      ; add s1 to those subsets.
      ; These
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (set)
                        (cons (car s) set)) rest)))))

(module+ test
  )