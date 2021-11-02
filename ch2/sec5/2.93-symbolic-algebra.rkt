#lang racket

(require "../sec4/2.4.2-tagged-data.rkt")
(require "../sec4/2.4.3-generic-table.rkt")
(require "2.93-generic-arithmetic.rkt")

; Installation package for polynomials
; notice that we are using the generic procedures add and mul.
; this means that we will be able to handle any type of coefficient.
; If we have a coercion method, then we can even add polynomials
; that that have different type coefficients (like complex vs rational)

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  ; Variable procedures from 2.3.2 - variables are just symbols
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  ; ⟨ procedures adjoin-term . . . coeff from text below ⟩

  ; Our termlist representation is good for sparse polynomials
  ; with many 0 coefficients
  ; Representation: list of nonzero terms, where each term is a lsit
  ; containing the order of the term and the coefficient for that order
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  ; Add polynomials
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  ; ⟨ procedures used by add-poly ⟩ -
  (define (add-terms L1 L2)
    ; If one of the termlists is just an empty list.
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             ; If the highest order term of one poly is greater than
             ; the other, then we don't have to add anything to the
             ; highest term
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     ; If the higest terms are the same order,
                     ; then we have to add their coefficients
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  ; Multiply polynomials
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  ; ⟨ procedures used by mul-poly ⟩

  ; Multiply two term lists together
  ; Idea: multiply each term of the first list
  ; by every term of the second list
  ; Resulting term lists are accumulated into a sum
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        ; If L1 is the empty termlist, product is also an empty termlist
        (the-empty-termlist)
        ; Multiply first term of L1 by all terms of l2,
        ; then add that resulting termlist to the resulting termlist
        ; from multiplying the rest of L1 by L2.
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  ; Multiply a single term by all the terms in a list
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        ; Get the first term, t2 of L
        (let ((t2 (first-term L)))
          (adjoin-term
           ; Add terms t1 and t2...
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           ; then adjoin this to the result of multiplying t1 by
           ; the rest of the terms list L
           (mul-term-by-all-terms t1 (rest-terms L))))))

  ; Negate terms
  (define (negate-terms L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((first (first-term L))
              (rest (rest-terms L)))
          (adjoin-term
           (make-term (order first) (negate (coeff first)))
           (negate-terms rest)))))

  ; Exercise 2.91: Polynomial division
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        ; Dividing a zero polynomial by another polynomial.
        ; zero quotient, zero remainder.
        (list (the-empty-termlist) (the-empty-termlist))
        ; Get the highest term of the dividend and divisor polynoms.
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              ; If the order of the divisor is higher than
              ; the order of the dividend, stop.
              ; Declare the dividend to be the remainder.
              (list (the-empty-termlist) L1)
              ; Get the coefficient and order of the first term
              ; in the quotient polynomial.
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                ; Build the remaining result.
                (let ((rest-of-result
                       (div-terms
                        (add-terms L1 (negate-terms
                                       (mul-terms L2
                                                  (list (make-term new-o new-c)))))
                        L2)))
                  ; Add the new term to the quotient of the
                  ; remaining result
                  (list
                   (adjoin-term (make-term new-o new-c) (car rest-of-result))
                   (cadr rest-of-result))))))))

  (define (div-poly p1 p2)
    ; We need to first check that the polynomials have the same variable
    (let ((var-p1 (variable p1))
          (var-p2 (variable p2)))
      (if (same-variable? var-p1 var-p2)
          (make-poly
           var-p1
           (car (div-terms (term-list p1) (term-list p2))))
          (error "polynomial division not supported for polynomials of different variables"
                 p1
                 p2))))

  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))

  ; Exercise 2.87
  ; A polynomial is zero if its term list is the empty list
  ; or all of its coefficients are zero.
  (define (=zero-polynomial? p)
    (define (=zero-termlist? tL)
      (if (empty-termlist? tL)
          #t
          (and (=zero? (coeff (first-term tL)))
               (=zero-termlist? (rest-terms tL)))))
    (let ((p-termlist (term-list p)))
      (or (empty-termlist? p-termlist)
          (=zero-termlist? (term-list p)))))

  (put '=zero? '(polynomial) =zero-polynomial?)

  ; Exercise 2.88: to subtract polynomials
  ; add to p1 the negation of p2.
  (define (negate-termlist tL)
    (if (empty-termlist? tL)
        tL
        ; Adjoin the negation of the first term to the negation
        ; of the rest of the terms
        (let ((first-tL (first-term tL))
              (rest-tL (rest-terms tL)))
          (adjoin-term
           (make-term (order first-tL) (negate (coeff first-tL)))
           (negate-termlist rest-tL)))))

  ; To negate a polynomial, we simply negate its termlist.
  (define (negate-poly p)
    (make-poly (variable p)
               (negate-termlist (term-list p))))
  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))

  ; To subtract , negate p2 first then add to p1.
  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))

  ; Exercise 2.93: gcd of two polynomials.
  (define (gcd-poly a b)
    (if (same-variable? (variable a) (variable b))
        (make-poly
         (variable a)
         (gcd-terms (term-list a) (term-list b)))
        (error "gcd not defined for polynomials in different variables" a b)))

  (define (remainder-terms a b)
    (cadr (div-terms a b)))

  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))))

  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(install-polynomial-package)

(define (make-polynomial var term-list)
  ((get 'make 'polynomial) var term-list))

(provide make-polynomial)

(module+ test
  (define p1 (make-polynomial 'x '((2 1) (0 1))))
  (define p2 (make-polynomial 'x '((3 1) (0 1))))
  (greatest-common-divisor p1 p2)
  )