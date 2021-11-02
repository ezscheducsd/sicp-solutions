#lang racket

; Assignment order matters.
; Define a procedure f such that evaluating
; (+ (f 0) (f 1))
; will return 0 if the arguments to + are evaluated from
; left to right
; but will return 1 if the arguments are evaluated from right to left

; Idea: keep an internal variable x-prev
; in f.
; When f is called with x,
; return the current value of prev, but also set x-prev to x.

(define f
  (let ((x-cur 0))
    (lambda (x)
      (let ((x-cur-copy x-cur))
        (begin
          (set! x-cur x)
          x-cur-copy)))))

(module+ test
  ; Left to right evaluation
  (define x1 (f 0))
  (define y1 (f 1))
  (+ x1 y1)

  ; Right to left evaluation
;  (define x2 (f 1))
;  (define y2 (f 0))
;  (+ x2 y2)
  )