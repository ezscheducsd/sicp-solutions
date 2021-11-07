#lang racket

; By testing the signs of the endpoints
; of intervals, it is possible to break mul-interval
; into nine cases, only one of which requires more than
; two multiplications.
(require "2.1.4-interval.rkt")

(define (nonnegative? x)
  (not (negative? x)))

; Multiplication by cases
(define (mul-interval-cases i1 i2)
  (let ((li1 (lower-bound i1))
        (li2 (lower-bound i2))
        (ui1 (upper-bound i1))
        (ui2 (upper-bound i2)))
    (cond
      ; (+ +)
      ((and (nonnegative? li1) (nonnegative? ui1))
       (cond
         ; (+ +)(+ +)
         ((and (nonnegative? li2) (nonnegative? ui2))
          (make-interval (* li1 li2) (* ui1 ui2)))
         ; (+ +)(- +)
         ((and (negative? li2) (nonnegative? ui2))
          (make-interval (* ui1 li2) (* ui1 ui2)))
         ; (+ +)(- -)
         (else
          (make-interval (* ui1 li2) (* li1 ui2)))))
      ; (- +)
      ((and (negative? li1) (nonnegative? ui1))
       (cond
         ; (- +)(+ +)
         ((and (nonnegative? li2) (nonnegative? ui2))
          (make-interval (* li1 ui2) (* ui1 ui2)))
         ; (- +)(- +)
         ((and (negative? li2) (nonnegative? ui2))
          (mul-interval i1 i2))
         ; (- +)(- -)
         (else
          (make-interval (* ui1 li2) (* li1 li2)))))
      ; (- -)
      (else
       (cond
         ; (- -)(+ +)
         ((and (nonnegative? li2) (nonnegative? ui2))
          (make-interval (* li1 ui2) (* ui1 li2)))
         ; (- -)(- +)
         ((and (negative? li2) (nonnegative? ui2))
          (make-interval (* li1 ui2) (* li1 li2)))
         ; (- -)(- -)
         (else
          (make-interval (* ui1 ui2) (* li1 li2))))))))

(module+ test
  (require rackunit)
  (define ipp (make-interval 2 5))
  (define inp (make-interval -3 3))
  (define inn (make-interval -4 -2))
  (mul-interval-cases ipp ipp)
  (mul-interval-cases ipp inp)
  (mul-interval-cases ipp inn)
  (mul-interval-cases inp ipp)
  (mul-interval-cases inp inp)
  (mul-interval-cases inp inn)
  (mul-interval-cases inn ipp)
  (mul-interval-cases inn inp)
  (mul-interval-cases inn inn)
  )