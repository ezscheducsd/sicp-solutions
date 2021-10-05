#lang racket

(require "../sec4/2.4.2-tagged-data.rkt")
(require "2.5.1-generic-arithmetic.rkt")

; Special coercion table.
(define gtable (make-hash))
(define (get operation-type operand-type)
  (hash-ref gtable (list operation-type operand-type)))
(define (put operation-type operand-type v)
  (hash-set! gtable (list operation-type operand-type) v))

(define (put-coercion t1 t2 coerce)
  (put t1 t2 coerce))

(define (get-coercion t1 t2)
  (get t1 t2))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)