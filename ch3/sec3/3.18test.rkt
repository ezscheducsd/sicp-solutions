#lang sicp

(#%require "3.18.rkt")

; A list will contain a cycle if any of the pairs
; in the top-level list structure point to the same pair
; or a previous pair in the top-level list structure

(define x (list 1 2 3 4 5))
(set-cdr! (cddr x) x)
(contains-cycle? x)