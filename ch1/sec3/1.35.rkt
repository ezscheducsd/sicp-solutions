#lang racket

(require "1.3.3-fixed-point.rkt")

(fixed-point
 (lambda (x) (+ 1 (/ 1 x)))
 1.0)

; We get 1.6180327868852458