#lang racket

(require "2.2.3-sequences.rkt")
(require "2.36.rkt")

; vector: sequence of numbers
; matrix: sequence of vectors.

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; result of matrix times vector:
; vector t, where
; t_i = sum_j m_ij*v_j
; in other words, t_i is the dot product
; of the vector m_i and v
(define (matrix-*-vector m v)
  (map (lambda (mi) (dot-product mi v)) m))

; Transpose matrix: columns are now the rows.
; using accumulate-n.
; get the first elements of each matrix horizontal vector
; (get the elements that will go into the first row)
(define (transpose mat)
  (accumulate-n
   cons null mat))

; matrix matrix multiplication.
; first we will transpose n to get easily accessible columns
; of n.
; each row in the final result should be a list
; of dot products of the current row of m with each row in
; cols (transpose n)
; For each vector in m, we will get its dot products
; with each vector in cols.
; A list of the dot products should be the result for
; each mapping.
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
     (lambda (mi)
       (matrix-*-vector cols mi))
     m)))

(module+ test
  (define identity-3 (list
                      (list 1 0 0)
                      (list 0 1 0)
                      (list 0 0 1)))
  (define v (list 4 5 6))
  (matrix-*-vector identity-3 v)
  (define random-3 (list
                    (list 23 54 13)
                    (list 78 12 10)
                    (list 7 99 100)))
  (transpose identity-3)
  (transpose random-3)
  (matrix-*-matrix identity-3 random-3)
  )