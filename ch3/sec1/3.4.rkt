#lang racket

; state variable - if the account is accessed more than seven
; consecutive times with an incorrect password, it invokes the procedure
; call-the-cops

(define (make-account balance password)
  (let ((attempts-remaining 7))
    (define (call-the-cops)
      (error "WEE WOO WEE WOO Suspicious activity detected."))
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
          (begin
            (set! attempts-remaining 7)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request: MAKE-ACCOUNT"
                               m))))
          ; If the password is incorrect, then whenever they call the
          ; returned function, it returns a complaint.
          ; If there are no more attempts left, call the cops.
          (if (zero? attempts-remaining)
              (call-the-cops)
              (begin
                (set! attempts-remaining (- attempts-remaining 1))
                (lambda (a) "Incorrect password")))))
    dispatch))

(module+ test
  (require rackunit)
  (define acc (make-account 100 'abc))
  (check-eq? ((acc 'withdraw 'abd) 0) "Incorrect password")
  (check-eq? ((acc 'withdraw 'abd) 0) "Incorrect password")
  (check-eq? ((acc 'withdraw 'abd) 0) "Incorrect password")
  (check-eq? ((acc 'withdraw 'abd) 0) "Incorrect password")
  (check-eq? ((acc 'withdraw 'abd) 0) "Incorrect password")
  (check-eq? ((acc 'withdraw 'abd) 0) "Incorrect password")
  (check-eq? ((acc 'withdraw 'abd) 0) "Incorrect password")
  (check-exn exn? (lambda ()
                   (check-eq? ((acc 'withdraw 'abd) 0) "Incorrect password")))
  ; reset attempts to 7 after correct password
  (check-eq? ((acc 'withdraw 'abc) 0) 100)

  ; Fails again after 8 more failed attempts
  (check-eq? ((acc 'withdraw 'abd) 0) "Incorrect password")
  (check-eq? ((acc 'withdraw 'abd) 0) "Incorrect password")
  (check-eq? ((acc 'withdraw 'abd) 0) "Incorrect password")
  (check-eq? ((acc 'withdraw 'abd) 0) "Incorrect password")
  (check-eq? ((acc 'withdraw 'abd) 0) "Incorrect password")
  (check-eq? ((acc 'withdraw 'abd) 0) "Incorrect password")
  (check-eq? ((acc 'withdraw 'abd) 0) "Incorrect password")
  (check-exn exn? (lambda ()
                   (check-eq? ((acc 'withdraw 'abd) 0) "Incorrect password")))
  )