#lang racket

; Show the environment structures created by evaluating
; (factorial 6) using each version of the factorial procedure
; as defined below

; Recursive
(define (factorial n)
  (if (= n 1) 1 (* n (factorial (- n 1)))))

; Iterative
(define (factorial-2 n) (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

; Recusrive environment structure
; Global environment G:
; - various arithmetic procedures +, *, -, etc
; - factorial: code and pointer to G

; Evaluating (factorial 3)
; new environment E1
; Frame F1 consisting of:
; - n bound to 3
; - points to G, which is factorial's environment
; Now we evaluate the body, which would
; fail the if condition and evaluate
; (* 3 (factorial 2))

; Now we need to evaluate (factorial 2)
; new environment E2
; Frame F2 consisting of:
; - n bound to 2
; - points to G, which is factorial's environment
; Now we evaluate the body, which would fail
; the if condition and evaluate
; (* 2 (factorial 1))

; Now we need to evaluate (factorial 1)
; new environment E3
; Frame F3 consisting of:
; - n bound to 1
; - points to G, which is factorial's environment
; Now we evaluate the body, which would pass the if condition
; and return 1.

; We used this to evaluate the
; the expression in E2, (* 2 1)
; This returns 2

; We use this to evaluate the expression in E1,
; (* 3 2)
; This returns 6, and the evaluation is complete.


; I'm kinda lazy, but the same idea applies to factorial iteration.