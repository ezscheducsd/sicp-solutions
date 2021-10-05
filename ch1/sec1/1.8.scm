#lang racket
; Exercise 1.8: Newton’s method for cube roots is based on
; the fact that if y is an approximation to the cube root of x,
; then a better approximation is given by the value
; x/y^2 + 2y
; /
; 3
; Use this formula to implement a cube-root procedure anal-
; ogous to the square-root procedure.

(define fraction (/ 1.0 1000))

(define (good-enough? guess x)
    (<
        (abs (- guess (improve-guess guess x)))
        (* fraction guess)))

(define (improve-guess guess x)
    (/  (+  
            (/ x (* guess guess))
            (* 2.0 guess))
        3.0))

(define (cube-root guess x)
    (if (good-enough? guess x)
        guess
        (cube-root (improve-guess guess x) x)))


(cube-root 10 64)