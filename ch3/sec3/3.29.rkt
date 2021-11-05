#lang sicp

(#%require "circuits.rkt")

; Another way to construct an or-gate is
; a compound digital logic device, built from and gates
; and inverters

; or in terms of and/inverters

; or(a1 a2) = not(not(or a1 a2))
; = not(and(not(a1) not(a2)))
; If a1 and a2 are both false, then the result is true.
; If either a1 or a2 is true, then the result if false.
; We want the opposite of this.

; So this or gate takes in a1, a2 and an output wire
; we want to sent each of the inputs to an inverter,
; then send both results to an and,
; then send the result of the and to another inverter.

(#%provide or-gate-2)
(define (or-gate-2 a1 a2 output)
  (let*
      ; First, we have build the wires for first two
      ; inverter outputs
      ((na1 (make-wire))
       (na2 (make-wire))
       ; First two inverters
       (inverter-a1 (inverter a1 na1))
       (inverter-a2 (inverter a2 na2))
       ; And the result of both inversions
       (and1 (make-wire))
       (and-gate-and1 (and-gate na1 na2 and1))
       ; Final inverter
       (inverter-r (inverter and1 output)))
    'ok))

; The delay time of the or-gate is
; inverter-delay + and-delay + inverter delay

; Notes:
; Is this all we have to do (do need to add an action?)
; By making an inverter from a1 and na1,
; we add an action to a1 to invert the signal of
; na1 when a1 changes.
; The same happens with a2 and na2.
; By building an and gate from na1, na2, and and1,
; We add actions to na1 and na2 to change the value
; of and1 when either na1 or na2 change.
; By building an inverter from and1 and output,
; we add the action to and1 to change the signal
; of output whenever and1 changes.