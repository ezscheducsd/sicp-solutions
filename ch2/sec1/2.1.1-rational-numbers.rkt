#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (abs x)
  (if (< x 0)
      (* -1 x)
      x))

; Normalizing sign for both positive
; and negative numbers:
; If numer and denom have same sign,
; make the normalized numer and denom positive.
; If opposite signs, make only the numer negative.
; If same sign, product is >= 0.
; If opposite signs, product is < 0
(define (make-rat n d)
  (define (sign x)
    (if (< x 0)
        -
        +))
  (let ((g (gcd n d)))
    (cons ((sign (* n d)) (abs (/ n g)))
          (abs (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(provide make-rat
         numer
         denom)