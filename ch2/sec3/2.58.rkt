#lang racket

; Differentiation program is defined in terms of abstract data
; we can modify it to work with different representations
; of expressions by solely changing the predicates,
; selectors, and constructors

; a. differentiate algebraic expressions in infix form,
; such as (x + (3 * (x + (y + 2))))
; Assume that + and * always take two arguments
; and that expressions are fully parenthesized

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
;        ((exponentiation? exp)
;         (make-product
;          (make-product (exponent exp)
;                        (make-exponentiation
;                         (base exp)
;                         (- (exponent exp) 1)))
;          (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))

; variables are symbols
(define (variable? x) (symbol? x))

; two variables are the same if symbols representing them
; are eq?
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; A sum expression is represented as a list containing
; addend, '+, and augend in that order
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

; sum simplification:
; 0, both numbers
(define (make-sum addend augend)
  (cond ((and (number? addend) (number? augend))
         (+ addend augend))
        ((=number? addend 0) augend)
        ((=number? augend 0) addend)
        (else (list addend '+ augend))))

(define addend car)

(define (augend s) (caddr s))

; A multiplication expression is represented as
; a list containing the multiplier, '* and multiplicand
; in that order
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (=number? exp num) (and (number? exp) (= exp num)))

; simplification: if either is 0, just 0
; if 1, the other
(define (make-product multiplier multiplicand)
  (cond ((or (=number? multiplier 0)
             (=number? multiplicand 0))
         0)
        ((=number? multiplier 1) multiplicand)
        ((=number? multiplicand 1) multiplier)
        ((and (number? multiplier)
              (number? multiplicand))
         (* multiplier multiplicand))
        (else
         (list multiplier '* multiplicand))))

(define multiplier car)
(define (multiplicand p) (caddr p))


; b. Problem becomes substantially harder if we allow standard algebraic
; notation.
; drop unnecessary parentheses and assumes that multiplication is
; done before addition

; (x + 3 * (x + y + 2))
; idea: at the current level of parentheses, we can
; combine product expressions first
; So go through the list, and if we see a *, we
; combine the multiplier and multiplicand
; into a two-operation expression
; Then we replace the three elements with the


; We can do the same for parenthesized subexpressions
; I'm not yet sure yet how to make a constructor.
; Maybe we can take a series of alternating operands
; and operators?
; And validate that it is correct.

; And while we are constructing 


(module+ test
  (define infix-exp '(x + (3
                           * (x + (y + 2)))))
  ; deriv (x + (y + 2)) is simply 1
  ; deriv is 1 + 3*1 + 0 = 4
  (deriv infix-exp 'x)
  )