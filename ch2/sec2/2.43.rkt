#lang racket

;(flatmap
; (lambda (rest-of-queens)
;   ; For each candidate row position in the kth column,
;   ; adjoin a the new row-column position to set of positions.
;   (map (lambda (new-row)
;          (adjoin-position
;           new-row k rest-of-queens))
;        ; Generate the integer rows that the kth queen should
;        ; be placed on in the kth column.
;        (enumerate-interval 1 board-size)))
; (queen-cols (- k 1)))
;THIS is the original version.

;(flatmap
; (lambda (new-row)
;   (map (lambda (rest-of-queens)
;          (adjoin-position new-row k rest-of-queens))
;        (queen-cols (- k 1))))
; (enumerate-interval 1 board-size))

; This code generates the candidate solutions
; for a board of size k.
; First, we get an enumeration of all row numbers 1...board size.
; Then, for each row number:
; - We get all valid positionings for the previous k-1 queens.
; - Finally, we add to each valid set a new position consisting of
; the row number and the current value of k.

; This means that for each row, we will call queens-cols(k-1).
; This is as opposed to the previous solution, where we call
; queens-cols(k-1) once for the current value of k.

; Run time of original solution (queens-cols)
; queens-cols(k) involves:
; Tqc(k-1)
; board size * qc(k-1)
; Tqc(k - 1) + board-size*qc(k - 1)

; Run time of bad solution:
; qc(k) involves:
; board-size.
; For each row, call qc(k-1) and create
; new candidate solutions involving qc(k - 1) and
; the current row. Time takes qc(k - 1)
; board-size*Tqc(k - 1) + board-size * qc(k-1).

; For each level of k, it will take roughly board-size longer.
; So the second solution should take board-size^board-size longer.