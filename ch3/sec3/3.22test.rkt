#lang sicp

(#%require "3.22.rkt")
(#%require rackunit)

(define q (make-queue))
((q 'insert!) 1)
((q 'insert!) 2)
(q 'print)
((q 'insert!) 3)
((q 'insert!) 4)

(check-eq? (q 'delete!) 1)
(check-eq? (q 'delete!) 2)
(check-eq? (q 'delete!) 3)
(check-eq? (q 'delete!) 4)
(check-true (q 'empty?))