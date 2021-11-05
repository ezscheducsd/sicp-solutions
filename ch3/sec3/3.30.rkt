#lang sicp

(#%require "circuits.rkt")

; We would like to implement an n-bit ripple carry adder
; The procedure is given three lists of wires:
; The wires for the first binary number
; The wires for the second binary number
; The wires for the sum
; A single wire for the carry
; We need to make sure that the lists are the same length.

; A, B, S are the inputs and the sum, c is the carry out.
; The idea: we have an iterative procedure
; that takes in 2 addend wires, a carry-in wire, and
; a sum wire.
; If there is only one more wire in one of the input
; lists, then the carry out wire will be c
; If there is more than one input wire, then
; the carry out wire will be a new wire
; that we will pass onto the next full adder.
(define (ripple-carry-adder A B S c)
  ; Helper, recursive full adder creator
  (define (make-full-adder A-partial B-partial c-in S-partial)
    ; We first decide what the new carry out wire will be
    (let ((c-out
           (if (null? (cdr A-partial))
               c
               (make-wire))))
      ; Make the intermediate full adder
      (full-adder (car A-partial)
                  (car B-partial)
                  c-in
                  (car S-partial)
                  c-out)
      ; If there are no more full adders to make, finished.
      ; otherwise, make a new full adder. The current c-out
      ; is piped to the next full adder as the carry in
      (if (null? (cdr A-partial))
          'ok
          (make-full-adder (cdr A-partial)
                           (cdr B-partial)
                           c-out
                           (cdr S-partial)))))
  ; We start out with a dummy wire.
  ; FIXME: Do we need to set the initial signal to 0?
  (let ((c-in-dummy (make-wire)))
      (make-full-adder A B c-in-dummy S)))

(#%provide ripple-carry-adder)

; Delay? I will do this later.