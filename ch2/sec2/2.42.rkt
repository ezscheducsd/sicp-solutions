#lang racket

(require "2.2.3-sequences.rkt")

; Queens 8 procedure.
; write:
; empty-board
; safe?
; adjoin-position
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        ; We want to filter the candidates to make
        ; sure that there are no conflicts
        (filter
         (lambda (positions) (safe? k positions))
         ; For each valid solution of k-1 queens,
         ; create a new candidate solution containing k queens.
         ; So this flat map will contain all candidate
         ; k-queen solutions.
         (flatmap
          (lambda (rest-of-queens)
            ; For each candidate row position in the kth column,
            ; adjoin a the new row-column position to set of positions.
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 ; Generate the integer rows that the kth queen should
                 ; be placed on in the kth column.
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; The final result of this should be a list of
; sets of positions.
; Each set of positions can just be represented as a list.
; The positions in each set should not conflict with each other.

; So the empty board should be the empty set.
(define empty-board null)

; rest-of-queens will be a set of non-conflicting positions.
; To add a new position specified by new-row and k,
; just add it to the end.

; We will represent a position as a pair of a row and a column.
(define (make-pos row col)
  (cons row col))

; We will also want some selectors to get the row and column.
(define (get-row pos)
  (car pos))

(define (get-col pos)
  (cdr pos))

(define (adjoin-position row col positions)
  (append positions (list (make-pos row col))))

(define (same-pos? p1 p2)
  (and (= (get-row p1) (get-row p2))
       (= (get-col p1) (get-col p2))))

(define (same-row? p1 p2)
  (= (get-row p1) (get-row p2)))

(define (same-col? p1 p2)
  (= (get-col p1) (get-col p2)))

(define (same-diag? p1 p2)
  (= (abs (- (get-row p1) (get-row p2)))
     (abs (- (get-col p1) (get-col p2)))))

; A queen attacks another if it is in the same
; row, column, or diagonal.
; Conventionally, a queen does not attack itself.
(define (attacks? p1 p2)
  (and (not (same-pos? p1 p2))
       (or (same-row? p1 p2)
           (same-col? p1 p2)
           (same-diag? p1 p2))))

; Determine if the kth position.
; is safe from the previous k-1 positions.
; When testing positions against the kth position,
; we will also test if the kth position itself
; attacks the kth position.
; This should return false.
(define (safe? k positions)
  (let ((kth-pos (list-ref positions (- k 1))))
    (if (null?
         ; get all the attackers here.
         ; if there are none, then we are safe.
         (filter (lambda (pos) (attacks? pos kth-pos)) positions))
        #t
        #f)))

(module+ test
  (queens 5))