#lang sicp

(#%require "circuits.rkt")
(#%require "3.29.rkt")

(define a1 (make-wire))
(define a2 (make-wire))
(define out (make-wire))

(or-gate-2 a1 a2 out)

(probe 'a1 a1)
(probe 'a2 a2)
(probe 'out out)
(propagate)

(set-signal! a1 1)
(propagate)

(set-signal! a1 0)
(set-signal! a2 1)
(propagate)

(set-signal! a2 0)
(propagate)