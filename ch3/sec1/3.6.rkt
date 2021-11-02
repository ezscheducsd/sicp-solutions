#lang racket

; It is useful to be able to reset a random number generator to produce a sequence
; starting from a given value.
; Design a new rand procedure that is called with an argument
; that is either the symbol generate or the symbol reset and
; behaves as follows:

; rand generate produces a new random number
; rand reset new-value resets the internal state variable to the designated
; new value. Thus, by resetting the state, one can generate repeatable
; sequences.
; These are handy to have when testing and debugging programs that use
; random numbers.


; So first, obviously, we need some kind of
; rand procedure that maintains internal state.

; a is the action to take
; should be either 'generate or 'reset
(define (rand a)
  ; Set random to some initial seed
  (let ((rand-cur random-init))
    ; Need internal procedures corresponding to 'generate
    ; and 'reset
    (cond
      ; generate a new random number, set to rand-cur, and return
      ; that value
      ((eq? a 'generate)
       (begin (set! rand-cur (rand-update rand-cur))
              rand-cur))
      ; return a function that, when called with the new value,
      ; sets the current state to that new value.
      ((eq? a 'reset)
       (lambda (new-cur)
         (set! rand-cur new-cur)))
      ; Errors otherwise.
      (else
       (error "Unrecognized rand argument " a)))))