#lang sicp

(#%require "3.25.rkt")

(define t (make-table 'table))
(define table-insert!
  (lambda (val . keys)
    (insert t keys val)))
(define (table-lookup . keys)
  (lookup t keys))
(define (table-delete! . keys)
  (insert t keys false))
(table-insert! 'a 1 1) 
(table-lookup 1 1) 
(table-insert! 'b 2) 
(table-insert! 'c 3) 
(table-insert! 'c 2 3 4) 
(table-insert! 'd 2 3 5) 
(table-insert! 'e 2 3 6) 
(table-lookup 3 4) 
(table-lookup 2 3) 
(table-lookup 2 3 4) 
(table-lookup 1) 
(table-insert! 'x 1 1) 
(table-insert! 'y 2 3 4) 
(table-lookup 1 1) 
(table-lookup 2 3 4) 
(table-delete! 2 3 4) 
(table-lookup 2 3 4) 