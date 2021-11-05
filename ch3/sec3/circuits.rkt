#lang sicp

(#%require "agenda.rkt")

; Implementation of wires:
; get-signal: current signal of the wire
; set-signal!: change the value of the signal on the wire
; to the new value
; add-action!: procedure should be run whenever the signal on the wire
; changes value. This is the vehicle by which changes in signal value
; are communicated to other wires

; after-delay: takes a time delay and a procedure to be run and executes
; the given procedure after given time delay

(#%provide
 ; Circuits primitives and combinations
 inverter
 and-gate
 or-gate
 half-adder
 full-adder

 ; Wire related
 make-wire
 get-signal
 set-signal!
 add-action!
 
 ; agenda related
 probe
 propagate)

; The agenda for propagation system
(define the-agenda (make-agenda))

; Inverter implementation
(define (inverter input output)
  ; inverting action:
  ; get the current signal of the input wire to calculate the
  ; new output signal.
  ; after the delay, set the signal of the output wire
  ; to the new value
  ; Then, we add this action to the input wire, so that
  ; the inverting action will run when the input wire
  ; signal is changed
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

; Simple logical not in terms of 0 and 1.
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

; And gate implementation
(define (and-gate a1 a2 output)
  ; The action to be run when either of the inputs
  ; a1 or a2 change.
  (define (and-action-procedure)
    ; compute the new output from the current signals
    ; of the two inputs
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      ; after the and-gate delay, set the new signal
      ; value for the output.
      (after-delay
       and-gate-delay
       (lambda () (set-signal! output new-value)))))
  ; The action needs to be added to both wires
  ; because the output is subject to change in either wire.
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  ; If either signal is 0, return 0.
  ; Otherwise, return 1.
  (cond
    ((= 0 s1) 0)
    ((= 0 s2) 0)
    (else 1)))

; Exercise 3.28: or gate implementation
; Or-gate implementation
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay
       or-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  ; If either signal is 1, return 1.
  ; Otherwise, return 0.
  (cond
    ((= 1 s1) 1)
    ((= 1 s2) 1)
    (else 0)))

; half adder
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

; Full adder
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; Wire implementation.
; Dispatch procedure, two local state variables
; signal and action-procedures
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

; Procedures to access local operations on wires
(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

; All we need to implement now is the agenda, which
; contains a schedule of things to do.
; simulation is driven by the procedure
; propagate, which operates on the agenda, executing each procedure
; on the agenda in sequence.
; Items can be added to the agenda while simulation
; is executing. Propagate will continue running as long as there
; are items on the agenda
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

; place a probe on the wire.
; When the signal changes value, it should print a new
; signal value with the current time and a name that
; identifies the wire
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

; Adding to the agenda.
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

; Begin by initializing the agenda, specifying delays for primitive
; function boxes
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)