#lang sicp

(#%require "circuits.rkt")

; Internal procedure accept-action-procedure!
; specifies that when a new action procedure is added
; to a wire, the procedure is immediately run.

; Why is this initialization necessary?
; Trace through the example of the half adder
; above. How would the system's response differ
; if we only added the procedure, but didn't
; immediately execute it.

; So first, we make our wires for the half adder.
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

; When we run this, the system does not display anything
; the sum wire, since the action is not
; added immediately.
(probe 'sum sum)

; When we run this, the system does not display anything
; for the carry wire.
(probe 'carry carry)

; When we run this, we hook up the wires
; and add the internal wires.
; When we do this, we hook up the wires
; to some primitive circuit components
; such as the inverter, or gate, and and-gate.
; Each of these adds update actions to the
; input wires.
; However, for this exercise, we assume that we
; do not run the update action immediately after
; adding the wire.
; This means that none of the wire states will
; actually change until we set one of the inputs.
; either input-1 or input-2.
(half-adder input-1 input-2 sum carry)

; Basically, some problems happen if we
; never set the signal of an input wire to a different value.
; If we just add the action to the wire but never actually
; run it, then no initial propagation will be performed.
; This means that all wires in the system, no matter where they are,
; will initially start out at 0.
; For example, if we simply build an inverter with a new wire,
; but never change the initial signal of the wire from 0 to 1,
; then the output of the inverter will just stay at 0, even
; though not 0 = 1. So the output of the inverter is logically
; incorrect.
