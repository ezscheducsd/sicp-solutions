#lang racket

; modify make-account so that is creates password protected accounts.
; it should process a request only if it is accompanied by the password and
; should otherwise return a complaint.

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m p)
    ; need to check the password first
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT"
                           m)))
        ; If the password is incorrect, then whenever they call the
        ; returned function, it returns a complaint.
        (lambda (a) "Incorrect password")))
  dispatch)

(provide make-account)

(module+ test
  (require rackunit)
  (define acc1 (make-account 100 'pass123))
  (check-eq? 100 ((acc1 'withdraw 'pass123) 0))
  ((acc1 'deposit 'pass123) 100)
  (check-eq? ((acc1 'withdraw 'pass123) 0) 200)
  (check-eq? "Incorrect password" ((acc1 'withdraw 'pass12) 0))
  )