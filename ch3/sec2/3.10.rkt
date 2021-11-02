#lang racket

; I DID THIS ON PAPER.
; SEE YOUR NOTEBOOK IF YOU WANT TO SEE THE FULL ANSWER.

; Using let to create a local state variable explicitly,
; as follows:

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

; Recall from Section 1.3.2 that let is simply syntatic sugar for
; a procedure call:

;(let ((<var> <ex[>)) <body>)
; is equivalent to
;((lambda (<var>) <body>) <exp>)

; This means that make-withdraw is equivalent to
(define (make-withdraw-equiv initial-amount)
  ; Applying make-withdraw to an arg will make a new environment E1
  ; with a frame that points to global environment
  ((lambda (balance)
     ; applying this lambda (with param procedure)
     ; will make a new environment E2 with a frame
     ; that points to E1 (since this lambda is evaluated under E1)
     (lambda (amount)
       (if (>= balance amount)
           (begin (set! balance (- balance amount))
                  balance)
           "Insufficient funds")))
   initial-amount))

; Use the environment model to analyze this alternate version of
; make-withdraw, drawing figures like the ones above to illustrate the interactions
(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))

; show that two version of make-withdraw create objects with
; the same behavior. How do the environment structures differ
; for the two version?

; DO THIS AT HOME DUDE. Draw out the procedures.


; The difference between the two version is that since we have a let expression,
; we are applying the value of another lambda expression (at the let level).
; So this means that (make-withdraw 100) will create an environment E1 with
; intial-amount bound to 100, then we will create another environment E2 with
; balance bound to initial amount, which is 100.
; Then W1 will be a procedure in the global environment, and the environment
; part of the procedure will point to E2.

; So essentially, for each make-withdraw object we create, there is an extra frame.