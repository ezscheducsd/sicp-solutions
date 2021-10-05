#lang racket

; A binary mobile has two branches, left and right.
; Each branch is a rod with certain length.
; From the rod hangs a weight or another binary mobile.
(define (make-mobile left right)
  (cons left right))

; Each branch has a length and a structure.
; The structure may be a weight or another mobile.
; if it is a weight, then 
(define (make-branch length structure)
  (cons length structure))

; a.
; left-branch of a mobile: just the first element (car)
; right-branch of a mobile: first elem of remaining list (car cdr)
; same respective functions for the branches.
(define left-branch car)
(define right-branch cdr)
(define branch-length car)
(define branch-structure cdr)

; b. total weight of a mobile?
; The weight of a mobile is the weight of its left branch
; plus the weight of its right branch.
; The weight of a branch is the weight of its structure.
; If the structure is just a weight (as a number), then return that.
; Otherwise, return the weight of the mobile structure hanging
; from that branch.
(define (is-not-mobile? structure)
  (not (pair? structure)))

(define (weight-branch branch)
  (if (is-not-mobile? (branch-structure branch))
      (branch-structure branch)
      (weight-mobile (branch-structure branch))))

(define (weight-mobile mobile)
  (+ (weight-branch (left-branch mobile))
     (weight-branch (right-branch mobile))))

; Test whether a binary mobile is balanced.
; length left branch * weight left branch
; = length right branch  * weight right branch.
; First, just get the weights of each branch first.
; Then see if the products are equal.

; Ok, so we need to check if the submobiles are balanced as well.
; So, first check if the sub-branches are balanced.
; Then check if the torque of a branch is equal to the
; torque of the right branch.

; The torque of a branch is the weight of a branch
; times its length.
(define (torque branch)
  (* (branch-length branch) (weight-branch branch)))

; A branch is balanced if its structure is a weight
; or its sub-mobile is balanced.
(define (branch-is-balanced branch)
  (let ((structure (branch-structure branch)))
    (or (is-not-mobile? structure)
        (mobile-is-balanced structure))))

; A mobile is balanced if its two branches are balanced
; And the torque of each branch is equal.
(define (mobile-is-balanced mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (branch-is-balanced left)
         (branch-is-balanced right)
         (= (torque left) (torque right)))))

; If we were to change the representation of mobiles
; and branches to cons two base values,
; then all we would need to do is change the accessors.

; in fact, all we need to change is the accessor to the
; right values.
; and also the test to see whether a structure is a
; mobile or a weight.
; A structure is a mobile if it is just a pair (as opposed to a list).
; it is not a mobile if it is just an integer weight.
; So we need not change very much.

(module+ test
  (require rackunit)
  (define b-length 10)
  (define w4 4)
  (define w8 8)
  (define w16 16)
  (define l10 10)
  (define l8 8)
  (define left-right-mobile
    (make-mobile
     (make-branch l10 w4)
     (make-branch l10 w4)))
  (define left-left-weight w8)
  (define left-mobile
    (make-mobile
     (make-branch l10 left-left-weight)
     (make-branch l10 left-right-mobile)))
  (define right-weight w16)
  (define test-mobile
    (make-mobile
     (make-branch l10 left-mobile)
     (make-branch l10 right-weight)))
  (check-true (mobile-is-balanced test-mobile))
  
  )