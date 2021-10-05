#lang racket

(require "2.2.3-sequences.rkt")

(provide accumulate-n)

; accumulate-n: given a sequence of sequences,
; accumulate the first elements of each sequence,
; then the second elements, and so on.
; the results are returned in a list.
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ; If the first sequences is empty, we are done.
      null
      ; The first accumulation value should be accumulating
      ; the first element of each sequence.
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(module+ test
  (accumulate-n + 0 (list
                     (list 1 2 3)
                     (list 4 5 6)
                     (list 7 8 9)
                     (list 10 11 12)))
  )