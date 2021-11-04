#lang sicp

(define (lookup key-1 key-2 table)
  (let ((subtable
         (assoc key-1 (cdr table))))
    (if subtable
        ; Found the subtable
        (let ((record
               ; Find the record if it exists.
               (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        ; Did not find the subtable, FAIL.
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        ; Found the subtable corresponding to key-1.
        ; Insert the record using the same method
        ; as the 1-d insert.
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        ; If there is no subtable existing for key-1.
        ; Create a new backbone pair at the upper table level.
        ; - its cdr is the cdr of the table
        ; - its car is the new subtable consisting of single
        ; new key-value pair.
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)