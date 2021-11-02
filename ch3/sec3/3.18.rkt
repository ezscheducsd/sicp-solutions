#lang sicp

; write a procedure that examines a list
; and determines whether it contains a cycle

; detect cycle?
; We can use a similar strategy as the previous exercise.
; But this time, we aren't doing any recursive calls
; for the car. Just for the cdr.

; Basically, we keep checking cdr until we get to the end,
; or if we come across a list that has already been counted.
(#%require "3.17.rkt")

(define (contains-cycle? x)
  (let ((seen-pairs '()))
    (define (helper l)
      (cond
        ; an empty list does not contain a cycle.
        ((null? l) #f)
        ; If the current list (a pair)
        ; has already been seen, then return true
        ((contains-pair? l seen-pairs) #t)
        ; Continue for the rest of the list.
        (else
         (begin
           (set! seen-pairs (add-pair l seen-pairs))
           (helper (cdr l))))))
    (helper x)))

(#%provide contains-cycle?)