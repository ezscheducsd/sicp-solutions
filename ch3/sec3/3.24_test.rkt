#lang sicp

(#%require "3.24.rkt")
(#%require rackunit)

(define t (make-table eq?))
(check-false ((t 'lookup-proc) 4))
((t 'insert-proc!) 1 2)
((t 'insert-proc!) 2 4)
(check-equal? ((t 'lookup-proc) 1) 2)
(check-equal? ((t 'lookup-proc) 2) 4)