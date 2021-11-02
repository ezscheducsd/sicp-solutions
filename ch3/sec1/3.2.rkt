#lang racket

; write a proc. make-monitored that takes
; as input a procedure f, that takes one input.

; Result returned by make-monitored is a third procedure,
; mf that keeps track of the number of times it has been called
; by maintaining an internal counter

; if input to mf is the symbol 'how-many-calls?
; then mf returns the value of the counter.
; if input is 'reset-count, mf resets counter to 0.
; For any other input, return the result of calling f on that input
; and increment the counter.

(define (make-monitored f)
  (let ((f-calls 0))
    (lambda (x)
      (cond
        ((eq? x 'how-many-calls?)
         f-calls)
        ((eq? x 'reset-count)
         (set! f-calls 0))
        (else
         (begin (set! f-calls (+ 1 f-calls))
                (f x)))))))

(module+ test
  (require rackunit)
  (define m-sqrt (make-monitored sqrt))
  (m-sqrt 'reset-count)
  (check-eq? 0 (m-sqrt 'how-many-calls?))
  (m-sqrt 0)
  (m-sqrt 1)
  (m-sqrt 2)
  (check-eq? 3 (m-sqrt 'how-many-calls?))
  (m-sqrt 'reset-count)
  (check-eq? 0 (m-sqrt 'how-many-calls?))
  )