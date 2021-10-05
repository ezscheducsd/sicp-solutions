#lang racket

; Exercise 2.89
; Define procedures that implement the term lsit representation described
; for dense polynomials.

; term lists of dense polynomials
; lists of coefficients
; Order of a term: length of the sublist beginning with the term's coefficient,
; decremented by 1

; example:
; (4 3 2 1)
; Order of the term with coefficient 4 is 4 - 1 = 3.

; Two adjoin a term, just adjoin it to the front
; Like is said in the book, we need to assume that
; the term being adjoined is the HIGHEST order term.
(require "2.5.1-generic-arithmetic.rkt")

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons (coeff term) term-list)))

; empty termlist is the same.
(define (the-empty-termlist) '())

; To get the first term...
; since the term list just holds coefficients, we
; need to calculate order according to the assumption made above.
(define (first-term term-list)
  (cons
   (- (length term-list) 1)
   (car term-list)))

(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

; 

; Term selectors stay the same.
(define (order term) (car term))
(define (coeff term) (cadr term))