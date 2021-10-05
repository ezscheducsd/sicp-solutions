#lang racket
; compute elements of pascal's triangle by a recursive process.
; procedure pascal takes arguments row, col

; Base cases: if column is 0,
; or we have col = row, then return 1.
; Assume that we are given a valid input.

; Otherwise, return pascal(row-1, col-1) + pascal(row-1, col)

(define (pascal row col)
    (if (or (= col 0) (= row col))
        1
        (+ 
            (pascal (- row 1) (- col 1)) 
            (pascal (- row 1) col))))


(pascal 0 0)
(pascal 1 0)
(pascal 1 1)
(pascal 2 1)
(pascal 3 1)
(pascal 3 2)
(pascal 3 3)
(pascal 4 0)
(pascal 4 1)
(pascal 4 2)
(pascal 4 3)
(pascal 4 4)