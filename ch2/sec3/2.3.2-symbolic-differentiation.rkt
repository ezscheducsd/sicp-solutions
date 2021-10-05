#lang racket

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
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation
                         (base exp)
                         (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))

; variables are symbols
(define (variable? x) (symbol? x))

; two variables are the same if symbols representing them
; are eq?
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(provide make-sum
         addend
         augend
         make-product
         multiplier
         multiplicand
         )

; Extended to work with arbitrary number of
; parts
; The addend can stay the same
; The augend can be the sum of the rest of the terms
; So to construct a sum, take an arbitrary number of
; parts

; make-sum should take at least two addends as arguments
(define (make-sum . as)
  (if (< (length as) 2)
      (error "make-sum requires at least two addends")
      ; To simplify, combine all the numeric
      ; values and isolate a list of non-numeric expressions
      (let ((numeric-sum
             (foldr + 0 (filter number? as)))
            (exps
             (filter
              (lambda (exp) (not (number? exp)))
              as)))
        (cond
          ; If there are no non-numeric expressions, just return the sum
          ((null? exps) numeric-sum)
          ; There is at least one non-numeric expression
          ; If the sum is 0, just make a sum from the expressions
          ((zero? numeric-sum)
           (if (= 1 (length exps))
               (car exps)
               (cons '+ exps)))
          ; numeric sum is also non-zero
          (else
           (append (list '+)
                   (list numeric-sum)
                   exps))))))

(define (=number? exp num) (and (number? exp) (= exp num)))

; sum is a lost whose first element is the symbol +
(define (sum? x) (and (pair? x) (eq? (car x) '+)))

; Addend as augend assume that s is a valid addition
; expression
; Addend: second item
(define (addend s) (cadr s))

; augend: sum of the rest of the items
; problem here: when doing this, we are giving to
; make-sum a LIST of the rest of the addends
; so it is like (make-sum (a2 a3 ...))
; instead of (make-sum a2 a3 ...)
(define (augend s)
  ; If there is only one more augend, just return
  ; that augend. Otherwise, make a sum out of the
  ; remaining augend
  (let ((rem-aug (cddr s)))
    (if (null? (cdr rem-aug))
        (car rem-aug)
        (cons '+ rem-aug))))

; exp. is a product if first element is the symbol *
(define (product? x) (and (pair? x) (eq? (car x) '*)))

; make a product: first element is * symbol
; simplification:
; we can combine numbers togeter
; If we get 0, then the entire thing is 0.
; If there are 1's, we can multiply as if there are no 1's

(define (one? x)
  (= 1 x))

; Idea: same as add
; isolate the numeric product
; isolate the non-numeric expressions
; combine as necessary
(define (make-product . ms)
  (if (< (length ms) 2)
      (error "make-product requires at least two multiplicands")
      ; To simplify, combine all the numeric
      ; values and isolate a list of non-numeric expressions
      (let ((numeric-prod
             (foldr * 1 (filter number? ms)))
            (exps
             (filter
              (lambda (exp) (not (number? exp)))
              ms)))
        (cond
          ; If there are no non-numeric exps, just return the numeric prod
          ((null? exps) numeric-prod)
          ; If the numeric prod is 0, just return 0
          ((zero? numeric-prod) 0)
          ; If the numeric prod is 1, make a prod expression
          ; from the remaining expressions
          ((one? numeric-prod)
           (if (one? (length exps))
               (car exps)
               (cons '* exps)))
          (else
           (append (list '*)
                   (list numeric-prod)
                   exps))))))

(define (multiplier p)
  (cadr p))

; If there is only one more multiplicand,
; just return that one multiplicand.
; Otherwise, make a product expression from the
; remaining
(define (multiplicand p)
  (let ((rem-mult (cddr p)))
    (if (one? (length rem-mult))
        (car rem-mult)
        (cons '* rem-mult))))


; Exercise 2.56:
; implement the differentiation rule iwth exponents
;  represent exponentiation expression
; as a list with carat (^) as the first symbol
(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '^)))

; simplification rules: anything raised to the power 0 is 1
; anything raised to the power 1 is itself
(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((and (number? base) (number? exp))
         (expt base exp))
        (else (list '^ base exp))))

; the base is the second element in the list
(define (base x)
  (cadr x))

; the exponent is the third element in the list
(define (exponent x)
  (cadr (cdr x)))


(module+ test
  (define var 'x)
  (define exp (make-sum
               (make-product 2 'x)
               4))
  (deriv exp var)
  (define exp2 (make-product
                2
                (make-exponentiation var 3)))
  (deriv exp2 var)
  (define exp3
    (make-product (make-product 2 var)
                  (make-product 3 (make-exponentiation var 2))
                  (make-sum (make-product 2 var)
                            4)))
  (deriv exp3 var)
  )