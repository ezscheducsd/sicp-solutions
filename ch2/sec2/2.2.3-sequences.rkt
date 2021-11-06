#lang racket

; Filter a sequence only to those elements
; that satisfy a given predicate.

(require sicp-helpers/mathlib)

(define (odd? x)
  (not (even? x)))

(provide accumulate enumerate-tree)
(provide enumerate-interval)

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Now we just need to implement enumeration.
; Enumerate all the leaves of a tree:
; Empty tree: null list.
; Leaf: single element list.
; Tree: append current subtree to rest of the subtree.
(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

; To enumerate an interval:
; null list if low > high.
; cons low (enumerate-interval low+1 high)
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low
            (enumerate-interval (inc low) high))))

;First, enumerate the leaves.
;Then only get the odds.
;Then square the results
;Then accumulate into a sum.
(define (sum-odd-squares tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

; nested mappings.
; Problem:
; given a positive integer n find all ordered pairs i, j
; where 1 <= j < i <= n such that i + j is prime.

; First, we want to generate all ordered pairs (i, j).
; First, get all 1...n.
; Then, for each i, we want to generate all 1 <= j < i.
; For each j, we want a pair (i, j)

; (enumerate-interval 1 n) - Gives us all our i's 1...n

;(map (lambda (i)
;       (map (lambda (j) (list i j))
;            (enumerate-interval 1 (- i 1))))
;     (enumerate-interval 1 n)))
; For each i, we are going to get a mapped list.
; Each list will contain pairs (i, j)
; Such that 1 <= j < i.

;(accumulate
; append null (map (lambda (i)
;                    (map (lambda (j) (list i j))
;                         (enumerate-interval 1 (- i 1))))
;                  (enumerate-interval 1 n)))
; Finally, we want to get all the pairs
; and put them into only one list (not nested within respective i.)
; So to do that we just append the lists together.

; Now we want to filter out pairs that have a prime sum.
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

; Finally, we want to generate a triple from each pair,
; where the third item is the sum of the previous two.
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

; general flatmap procedure
; This assumes that proc itself returns some
; kind of list.
(provide flatmap)
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(provide prime-sum-pairs)
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                           (lambda (i)
                             (map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))

; Procedure: generate permutations of a set (assuming)
; all items are distinct.
; Permutations generates all permutations for the set
; containing 1...n
; Idea:
; Recursively generate the permutations for S - x, where
; x is the first element.
; Then add x to the front of each of those permutations.

; The remove procedure can be represented as a filter,
; where we filter out any items in the sequence that are equal
; to the target item.

(define (permutations s)
  ; empty set?
  ; sequence containing empty set
  (if (null? s)
      (list null)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(module+ test
  (define n 10)
  (prime-sum-pairs 5)
  )