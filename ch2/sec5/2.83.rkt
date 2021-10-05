#lang racket

; tower of types (lowest to highest): rational -> real(scheme number) -> complex

; For rational and scheme number, we will define a procedure
; that raises it to the next type. See 2.5.2-generic-arithmetic-raise.rkt

(require "2.5.2-generic-arithmetic-raise.rkt")

(module+ test
  (require rackunit)
  (define r1 (make-rational 2 3))
  (raise r1)
  (raise (raise r1))
  )