#lang sicp

; multi-dimensional table
; There will be a top level backbone.

; each backbone pair will point to a table
; the table represents all the values associated with the key
; (which will be stored as a dummy pair)

; Needed functionality:
; - associate a single key with a value in a table
; - associate multiple keys with a value in a table
; - insert a single key, val pair into a table if it does not exist
; - insert multiple keys with a value into a table if it does not exist

; Each table will have a value and a backbone list.
; The car of a table is its key
; The cdr of a table is a pair consisting of:
; - the value (default false)
; - the backbone list where each entry points to a subtable

; I wrote a diagram representation in my notebook. Refer to that
; if you are confused.

; A table has 3 parts:
; the key
; the value
; the subtables
(define (make-table key)
  (cons key (cons false nil)))

(define (table-key table) (car table))
(define (table-val table) (car (cdr table)))
(define (table-subtables table) (cddr table))

(define (table-set-subtables! table subtables)
  (set-cdr! (cdr table) subtables))
(define (table-set-val! table val)
  (set-car! (cdr table) val))

(define (memf proc lst)
  (cond
    ((null? lst) #f)
    ((proc (car lst)) (car lst))
    (else
     (memf proc (cdr lst)))))

; lookup is implemented in terms of assoc.
(define (lookup table keys)
  (cond
    ((null? keys) (error "Table lookup requires at least one key."))
    (else
     (let ((target-table (assoc table keys)))
       (if (not target-table)
           false
           (table-val target-table))))))

; Assoc will always receive at least one key
; Look in the table's
; backbone list for a matching first key in a subtable.
; If we don't find the matching key, return false
; If we find the matching table, there might be more keys
; to search with.
; - if there was only one key, then return the table
; - if there is more than one key:
; Call assoc on the found table and the rest of the keys
(define (assoc table keys)
  (let* ((first-key (car keys))
        (rest-keys (cdr keys))
        (target-table (memf
                       (lambda (table)
                         (equal? first-key (table-key table)))
                       (table-subtables table))))
    (cond
      ; If the target table is false, return false
      ((not target-table) false)
      ; If there is only one more key, just return the table
      ((null? rest-keys) target-table)
      ; We will have to search for the rest of the keys
      ; in this table
      (else
          (assoc target-table rest-keys)))))

; To insert:
; If there is only one arg, fail (there needs to be at least a key and a val)
; - First use assoc to locate the "table" with the key.
; If we get the table, set the value of the table to the new value.
; If we get false, then we will have to insert the new value
; into the table and associate with the keys.
; If we get false, all we know is that there is no table corresponding
; to all the given keys.

; Now we are inserting a new value.
; At the current table, search for the table corresponding to the
; first key.
; If we find it, and there was only one key left, then
; set the value of that table to the new value
; If we find it, and there is more than one key left,
; then insert the remaining keys and value into that table

; If we don't find the corresponding table,
; create a new table with the first key, default false value,
; and empty subtables.
; If there is only one key left, set the value of the new table
; If there is more than one key left, insert the remaining keys
; and value into that table.

(define (insert table keys val)
  (cond
    ((null? keys) (error "Insert requires at least one key: " keys))
    (else
     (let* ((first-key (car keys))
            (rest-keys (cdr keys))
            (target-table (assoc table (list first-key))))
       (cond
         ; Target table not found. We will have to
         ; create a new table and add it to the subtables of the current
         ; table
         ((not target-table)
          (let ((new-table (make-table first-key)))
            (table-set-subtables!
             table
             (cons new-table (table-subtables table)))
            (if (null? rest-keys)
                (table-set-val! new-table val)
                (insert new-table rest-keys val))))
         ; Target table found.
         (else
          ; If there are no more keys, set the value of this table.
          (if (null? rest-keys)
              (table-set-val! target-table val)
              (insert target-table rest-keys val))))))))

(#%provide assoc make-table lookup
           insert)