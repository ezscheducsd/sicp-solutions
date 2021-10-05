#lang racket

(require "../sec4/2.4.2-tagged-data.rkt")
(require "2.5.2-coercion.rkt")

; Table operations
(define gtable (make-hash))
(define (get operation-type operand-type)
  (hash-ref gtable (list operation-type operand-type)))
(define (put operation-type operand-type v)
  (hash-set! gtable (list operation-type operand-type) v))

(provide get put)

; Generic application that looks up the correct procedure
(define (apply-generic op . args)
  ; type-tags: note that this is a list
  ; this is for generic operations that might take more than
  ; one type tag.
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))


; We will be adding/modifying:
; - coercion procedures to generic arithmetic package
; - apply-generic - doesn't try coercion of two arguments
; have the same type
; - apply-generic - handles coercion in the case of multiple arguments

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(provide add sub mul div equ? =zero?
         make-scheme-number
         make-rational
         make-complex-from-real-imag
         make-complex-from-mag-ang)

; Scheme number package.
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  ; Ex. 2.79: equ for scheme numbers
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  ; Ex 2.80: =zero? for scheme numbers
  (put '=zero? '(scheme-number) zero?)
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; Rational number package
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))

  ; two rational numbers have the same sign if
  ; numers are both 0 or, when multiplied by each other,
  ; the result is positive.
  ; The resulting product is positive if, when multiplying numer
  ; by denom, results in a positive.
  (define (same-sign? x y)
    (or (and (zero? (numer x)) (zero? (numer y)))
        (let ((prod (mul-rat x y)))
          (positive? (* (numer prod) (denom prod))))))

  ; Ex. 2.79: equ for rational numbers
  ; rational numbers are equal if they have the same sign,
  ; absolute value of numerators are equal, and absolute value of
  ; denom is equal.
  (define (equ? x y)
    (and (same-sign? x y)
         (= (abs (numer x)) (abs (numer y)))
         (= (abs (denom x)) (abs (denom y)))))

  ; Ex 2.80: a rational number is zero if its numerator is zero
  (put '=zero? '(rational)
       (lambda (x) (zero? (numer x))))

  (put 'equ? '(rational rational) equ?)
  
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

; complex number package.
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ; Exercise 2.79: two complex numbers are equal if their
  ; respective real part and imaginary parts are equal.
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))

  ; Exercise 2.80: a complex number is zero if both its
  ; real part and imaginary parts are both 0.
  (put '=zero? '(complex)
       (lambda (z1)
         (and (zero? (real-part z1))
              (zero? (imag-part z1)))))
  
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))

  ; complex component selectors (real-part, ang, etc.)
  ; in the complex number package so far only have
  ; generic table support for 'rectangular or 'polar,
  ; but not yet for 'complex numbers (in general)
  (put 'equ? '(complex complex) equ?)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  
  
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; package installation
(install-scheme-number-package)
(install-complex-package)
(install-rational-package)