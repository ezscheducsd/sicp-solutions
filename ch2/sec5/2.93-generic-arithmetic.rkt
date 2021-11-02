#lang racket

(require "../sec4/2.4.2-tagged-data.rkt")
(require "../sec4/2.4.3-complex-data-directed.rkt")
(require "../sec4/2.4.3-generic-table.rkt")

(define equal-types? eq?)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let* ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args))
                    (higher-type (higher a1 a2)))
                ; We need to get the higher type,
                ; raise the lower type to that type, then try again.
                (cond
                  ; Equal types, and the procedure does not exist. Do not try to change the types.
                  ; Just let it fail.
                  ((equal-types? type1 type2) (error "No method for these types" (list op type-tags)))
                  ((equal-types? higher-type type1)
                    (apply-generic op a1 (raise-to a2 type1)))
                  (else
                   (apply-generic op (raise-to a1 type2) a2))))
              (error "No method for these types"
                     (list op type-tags)))))))


; We will be adding/modifying:
; - coercion procedures to generic arithmetic package
; - apply-generic - doesn't try coercion of two arguments
; have the same type
; - apply-generic - handles coercion in the case of multiple arguments

; Exercise 2.84: Apply-generic where we can raise one type to the other.
; If we can't raise a type, then it should be the highest.
; raise t1 to t1'. If t1' is t2, then t2 is higher.
; try again for t1' and t2.
(define (higher x y)
  ; If we can't raise rx anymore, then return the type tag of x.
  ; Raise rx to rx'. If the tag of rx' is the tag of y, then y is higher.
  ; Otherwise, try again x, rx', y
  (define (iter-higher x rx y)
    (let* ((tx (type-tag x))
           (crx (contents rx))
           (ty (type-tag y))
           (raise-rx (get 'raise (list (type-tag rx)))))
      (cond ((false? raise-rx) tx)
            ((eq? ty (type-tag (raise-rx crx))) ty)
            (else
             (iter-higher x (raise-rx crx) y)))))
  (iter-higher x x y))

; Raise one type all the way to another type.
(define (raise-to x t)
  (let* ((tx (type-tag x))
         (cx (contents x)))
    ; If same type already, return the same.
    ; Otherwise, raise and try again.
    (if (equal-types? t tx)
        x
        (raise-to (raise x) t))))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))
(define (negate x) (apply-generic 'negate x))
(define (greatest-common-divisor x y)
  (apply-generic 'greatest-common-divisor x y))
(define (reduce x y)
  (apply-generic 'reduce x y))

(provide add sub mul div equ? =zero? raise negate greatest-common-divisor
         reduce
         higher
         make-scheme-number
         make-rational
         make-complex-from-real-imag
         make-complex-from-mag-ang
         raise-to)

; Scheme number package.
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))

  ; Ex 2.93: gcd of two scheme numbers
  (put 'greatest-common-divisor '(scheme-number scheme-number)
       (lambda (a b) (tag (gcd a b))))
  
  ; Ex. 2.79: equ for scheme numbers
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  ; Ex 2.80: =zero? for scheme numbers
  (put '=zero? '(scheme-number) zero?)

  ; Ex 2.83: raise a scheme number (real) to a complex number
  ; A complex number with real part equal to scheme number
  ; and imag part 0.
  (put 'raise '(scheme-number)
       (lambda (x) (make-complex-from-real-imag x 0)))

  ; Ex 2.88: Negating a scheme number.
  (put 'negate '(scheme-number)
       (lambda (x) (tag (- x))))

  ; Ex 2.97: Reducing two integers that might share a common factor
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))

  (put 'reduce '(scheme-number scheme-number)
       (lambda (x y)
         (let ((reduced (reduce-integers x y)))
           (list
            (tag (car reduced))
            (tag (cadr reduced))))))
  
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

  ; Exercise 2.93: rationals have generic numerators and denominators.
  ; Since we have generic numerators and denominators,
  ; we are going to need a generic gcd procedure.
  ; This, for now, is only going to be defined for regular scheme numbers
  ; and polynomials.

  ; Construtor is now generic.
  ; Exercise 2.97:
  ; We can now reduce numerator and denominator in generic cases.
  (define (make-rat n d)
    (let ((reduced (reduce n d)))
      (cons
       (car reduced)
       (cadr reduced))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))

  ; Be careful: we did not yet change same-sign?, equ?
  ; or raise to handle generic parts.

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

  ; Ex 2.83: Raising a rational number raises it to a scheme-number.
  (put 'raise '(rational)
       (lambda (x) (make-scheme-number (/ (numer x) (denom x)))))

  ; Ex. 2.88: negate a rational number.
  ; just negate its numerator.
  (put 'negate '(rational)
       (lambda (x) (tag (make-rat (negate (numer x))
                                  (denom x)))))

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

   ; Ex 2.88: to negate a complex number,
  ; Well, a complex number plus its negation should
  ; be equal to 0.
  ; So we can just negate the real part and the imag part.
  (define (negate-complex z1)
    (make-from-real-imag (- (real-part z1))
                         (- (imag-part z1))))
  (put 'negate '(complex)
       (lambda (z1) (tag (negate-complex z1))))
  
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

(module+ test
  (greatest-common-divisor 5 10)
  )