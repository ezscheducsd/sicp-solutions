#lang racket

;k-term continued fracion:
;cont-frac n d k
;k: the number of expanded terms in the continued fraciton.
;n: returns n_i given i
;d: returns d_i given i
;
;Base case: if current index is equal to k, just n_k over d_k

(require sicp-helpers/mathlib)

(provide cont-frac)

(define (cont-frac n d k)
  (define (helper i)
    (let ((ni (n i))
          (di (d i)))
      (if (= i k)
          (/ ni di)
          (/ ni (+ di (helper (inc i)))))))
  (helper 1))

(define (cont-frac-iter n d k)
  (define (helper frac i)
    (if (< i 1)
        frac
        (let ((ni (n i))
              (di (d i)))
          (helper (/ ni (+ di frac)) (- i 1)))))
  (helper 0 k))

(module+ test
  (define n (lambda (i) 1.0))
  (define d (lambda (i) 1.0))
  (/ 1 (cont-frac n d 100))
  (/ 1 (cont-frac-iter n d 100))
 )