#lang racket


; Tag data and get the contents of tagged data.

; Exercise 2.78: Take advantage of internal typing system.
(define (attach-tag type-tag contents)
  ; primitive number: don't attach anything.
  (if (number? contents)
      contents
      (cons type-tag contents)))

;(define (type-tag datum)
;  ; added condition: if it is a number, return
;  ; scheme-number symbol (so it works in the generic package)
;  (if (pair? datum)
;      (car datum)
;      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (type-tag datum)
  ; primitive number: 'scheme-number, so it can be used in generic package.
  (cond ((number? datum)
         'scheme-number)
        ((pair? datum)
         (car datum))
        (else (error "bad tagged datum: TYPE-TAG" datum))))

;(define (contents datum)
;  (if (pair? datum)
;      (cdr datum)
;      (error "Bad tagged datum: CONTENTS" datum)))
(define (contents datum)
  (cond
    ; primitive number w/out type: no tag to strip,
    ; so just return the number itself already
    ((number? datum) datum)
    ((pair? datum)
      (cdr datum))
    (else (error "Bad tagged datum: CONTENTS" datum) )))

(provide attach-tag type-tag contents)