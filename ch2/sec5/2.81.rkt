#lang racket

; Do we need to add procedures to coerce to the same type?

; a. what happens if, with louis's procedures installed, apply-generic
; is called with two arguments of type scheme-number or two arguments
; of type complex for an operation that is not found in the table for those types?
(define (equal-types? t1 t2) eq?)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      ; If the procedure exists for the types.
      (if proc
          (apply proc (map contents args))
          ; Procedure does not exist for the types
          ; Coercion resolving: 2 arguments.
          (if (= (length args) 2)
              ; Get the arguments and their respective type tags
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal-types? type1 type2)
                    (error "No method for these types" (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      ; If we can coerce first argument type to second, try apply-generic with that.
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else (error "No method for these types"
                                         (list op type-tags)))))))
              ; no method found for the 3+ arguments.
              (error "No method for these types"
                     (list op type-tags)))))))

; a. suppose we have a generic add procedure, but it is not defined for adding two complex numbers yet.
; so we try adding two complex numbers
; since the operation is not defined, and the arguments are of length 2, we try to coerce.
; There is a coercion procedure for complex to complex, so we first coerce the first argument to
; the same type (complex) then try apply-generic again for complex and complex.
; But we just did that, so there will be infinite recursion.

; b. does anything have to be done?
; Suppose that those procedures were not in the coercion table.
; Then we would try again to find the coercion procedures for the same type.
; But these don't exist, so we would get an error that the method does not exist for the types.
; So we didn't have to do anything in the first place.

; c. Don't try coercion if the two arguments have the same type.
