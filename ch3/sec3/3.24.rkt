#lang sicp

; current key comparison for table uses
; equal?
; This is not always the appropriate test.
; For instance, we might have
; a table w/ numeric keys in which we don't need an exact
; match to the number we're looking up, but
; only a number within some tolerance of it.

; Design a table construtor make-table
; that takes as an argument same-key? procedure
; that will be used to test "equality" of keys.

; should return a dispatch procedure that can be used
; to access lookup, insert! procedures

; user-provided same-key?
(define (make-table same-key?)
  ; dummy backbone pair.
  ; This is the initial table
  (let ((local-table (list '*table*)))
    ; The implementations of lookup and insert
    ; are essentially the same as the implementation
    ; for 1-d tables.
    ; The only difference is that both procedures use the
    ; free variable local-table.
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            false)))
    (define (assoc key records)
      (cond ((null? records) false)
            ; local assoc refers to the local same-key?
            ; procedure
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (insert! key value)
      (let ((existing-record (assoc key (cdr local-table))))
        (cond
          (existing-record
           (set-cdr! existing-record value)
           existing-record)
          (else
           ; create the new record and backbone pair
           ; cdr of backbone pair is the cdr of table
           ; set cdr of table to backbone pair
           (let ((new-pair (cons (cons key value) (cdr local-table))))
             (set-cdr! local-table new-pair))))))
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        (else (error "Unknown TABLE operaton: " m))))
    dispatch))

(#%provide make-table)