#lang racket

; Consider bank accounts with passwords of 3.3.
; Suppose our system requires ability to make
; joint accounts.

; define make-joint.
; three arguments
; - password protected account
; - matching password
; - third argument: new password

; make-join: create an additional access to the original
; account using the new password

; you may wish to modify solution to exercise 3.3
; to accomodate this new feature.

; Initial (naive?)
; solution.
; Create a new function similar to 3.3
; We have the original account and password
; as local variables.
; Check for the password first.
; If incorrect, throw an error.
; Otherwise, delegate to the original account.

(require "3.3.rkt")

(define (make-joint orig-account orig-password new-password)
  ; delegate withdrawal to original account
  (define (withdraw amount)
    ((orig-account 'withdraw orig-password) amount))
  (define (deposit amount)
    ((orig-account 'deposit orig-password) amount))
  (lambda (action password)
    (if (eq? new-password password)
        (cond
          ((eq? action 'withdraw)
           withdraw)
          ((eq? action 'deposit)
           deposit)
          (else
           (error "Unrecognized account action " action)))
        (lambda (a) "Incorrect password."))))

(module+ test
  (require rackunit)
  (define acc1 (make-account 100 'pass123))
  (check-eq? 100 ((acc1 'withdraw 'pass123) 0))
  ; joint account here.
  (define acc2 (make-joint acc1 'pass123 'pass321))
  (check-eq? 100 ((acc2 'withdraw 'pass321) 0))
  ((acc1 'deposit 'pass123) 100)
  (check-eq? ((acc2 'withdraw 'pass321) 0) 200)
  (check-eq? "Incorrect password." ((acc2 'withdraw 'pass421) 0))
  )