#lang sicp

(#%require "circuits.rkt")
(#%require "3.30.rkt")

(define a1 (make-wire))
(define a2 (make-wire))
(define a3 (make-wire))

(define b1 (make-wire))
(define b2 (make-wire))
(define b3 (make-wire))

(define s1 (make-wire))
(define s2 (make-wire))
(define s3 (make-wire))

(define c (make-wire))

(probe 's1 s1)
(probe 's2 s2)
(probe 's3 s3)
(probe 'c c)

(define A (list a1 a2 a3))
(define B (list b1 b2 b3))
(define S (list s1 s2 s3))

(ripple-carry-adder A B S c)

(set-signal! a1 1)
(set-signal! a2 0)
(set-signal! a3 1)

(set-signal! b1 0)
(set-signal! b2 1)
(set-signal! b3 1)

(propagate)