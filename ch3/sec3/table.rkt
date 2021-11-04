#lang sicp

; lookup procedure takes a key as an argument,
; returns the associated value or false
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

; assoc is a helper for lookup.
; assoc returns the record that has the given key as
; its car
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

; to insert a value in a table,
; first use assoc to see if there is already a record in the
; table with the key.
; If not, form a new record, insert this at the head of the
; table's list of record, after the dummy record
(define (insert! key value table)
  (let ((existing-record (assoc key (cdr table))))
    (cond
      (existing-record
       (set-cdr! existing-record value)
       existing-record)
      (else
       ; create the new record and backbone pair
       ; cdr of backbone pair is the cdr of table
       ; set cdr of table to backbone pair
       (let ((new-pair (cons (cons key value) (cdr table))))
         (set-cdr! table new-pair))))))

; table constructor: simply create a list
; containing the symbol *table*. This is the dummy pair.
(define (make-table)
  (list '*table*))

(#%provide make-table insert! lookup)