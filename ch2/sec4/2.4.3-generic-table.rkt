#lang racket

(require "2.4.2-tagged-data.rkt")

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

(define gtable (make-hash))
(define (get operation-type operand-type)
  (with-handlers
      ([exn:fail:contract? (lambda (exn) #f)])
    (hash-ref gtable (list operation-type operand-type))))


(define (put operation-type operand-type v)
  (hash-set! gtable (list operation-type operand-type) v))

(provide get put apply-generic)

(module+ test
  (define op1 'test-operation-1)
  (define op2 'test-operation-2)
  (define type1 'test-type-1)
  (define type2 'test-type-2)
  (define (f1 x) x)
  (define (g1 x) x)
  (define (f2 x) x)
  (define (g2 x) x)
  (put op1 type1 f1)
  (put op2 type1 g1)
  (put op1 type2 f2)
  (put op2 type2 g2)

  (get op1 type1)

  (get 1 2)
  )