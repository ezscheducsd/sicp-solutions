#lang racket

(require "2.4.3-generic-table.rkt")

; data-directed symbolic differentiation(define (deriv exp var)
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (variable? exp) (symbol? exp))
(define (same-variable? v1 v2)
  (and (and (variable? v1) (variable? v2))
       (eq? v1 v2)))

; a.
; Here, we keep the conditionals in the case where the expression is
; a number or a variable.
; Otherwise, we get the derivative procedure
; for the specific operation (+, -, *, etc.)
; Then apply that operation to the operands.
; We can't assimilate number? and variable? into the data directed
; dispatch because they are not expressions with operations (like add or multiply)
; The table contains specific deriv procedures for certain operations.


; b. Write the procedures for the derivatives of sums and products
; and auxiliary code required to install them in the table used
; by the program above.

; For sum: we need to PUT a derivative procedure
; corresponding to the symbol 'deriv and the operator '+

; We will assume prefix expressions (list of operator followed by operands)
; expressions have at least two operands
; The operations for make-sum, addend, augend
; make-prod, multiplier, multiplicand need to be at the top
; level, rather than in the installation packages.
; This is because the deriv rules of one operator
; might depend on another operator (ex: deriv of product involves addition)
(define (make-sum addend augend)
  (cond ((=number? 0 addend) augend)
        ((=number? 0 augend) addend)
        ((and (number? addend)
              (number? augend))
         (+ addend augend))
        (else
         (list '+ addend augend))))

(define (addend operands)
  (car operands))

(define (augend operands)
  (cadr operands))

(define (make-product multiplier multiplicand)
  (cond ((or (=number? 0 multiplier)
             (=number? 0 multiplicand))
         0)
        ((=number? 1 multiplier) multiplicand)
        ((=number? 1 multiplicand) multiplier)
        (else
         (list '* multiplier multiplicand))))

(define (multiplier operands)
  (car operands))
(define (multiplicand operands)
  (cadr operands))

(define (=number? n x)
  (and (number? x)
       (= n x)))

; This is our derivative procedure for sum
; remember - it's just the sum of the derivatives of each addend
; The specific derivative procedure takes a list of operands and a
; the relative variable.
(define (install-sum-deriv)
  (define (deriv-sum operands var)
    (let ((daddend
           (deriv (addend operands) var))
          (daugend
           (deriv (augend operands) var)))
      (make-sum daddend daugend)))
  (put 'deriv '+ deriv-sum)
  'done
  )

; Derivative procedure for product
; Assume that there are only two operands for now (and those operands are
; in a list)
; So we have operands a * b
(define (install-prod-deriv)
  (define (deriv-prod operands var)
    (let ((dmultiplier
           (deriv (multiplier operands) var))
          (dmultiplicand
           (deriv (multiplicand operands) var)))
      (make-sum (make-product (multiplier operands) dmultiplicand)
                (make-product (multiplicand operands) dmultiplier))))
  (put 'deriv '* deriv-prod)
  )

; Let's do the exponent derivative rule.
(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((and (number? base) (number? exp))
         (expt base exp))
        (else (list '^ base exp))))

; the base is the second element in the list
(define (base operands)
  (car operands))

; the exponent is the third element in the list
(define (exponent operands)
  (cadr operands))

(define (install-exponentiation-deriv)
  (define (deriv-exponentiation operands var)
    (let* ((ops-base (base operands))
           (ops-exponent (exponent operands))
           (dops-base (deriv ops-base var)))
      (make-product
       (make-product
        ops-exponent
        (make-exponentiation ops-base (- ops-exponent 1)))
       dops-base)))
  (put 'deriv '^ deriv-exponentiation)
  )

; d. If we reversed the order of the indexing into the table,
; all we would have to do is reverse the ordering of the put
; statement in each installation package.

(module+ test
  (install-sum-deriv)
  (install-prod-deriv)
  (install-exponentiation-deriv)
  (define var 'x)
  (define sum1 (make-sum 1 var))
  (deriv sum1 var)

  (define m1 (make-product 2 var))
  (define m2 (make-product 3 (make-exponentiation var 2)))
  (deriv (make-product m1 m2) var)
  )