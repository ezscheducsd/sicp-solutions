#lang racket

(require sicp-helpers/mathlib)

; This produces the answer list in the reverse order
; of the one desired because you are
; adding the square of the item to the front of
; the list, instead of to the back.
; This means that elements processed later
; in the original list will occur farther in front
; of the transformed list, and hence producing the
; reverse order.
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))

; So the idea here is that we are trying to
; add the square of the current item
; to the back of the list as mentioned above.
; But the problem is that when we combine
; the square of the current item with the
; rest of list, we are actually putting
; a list containing only two items
; into the updated final answer:
; the rest of the list squared, and the
; squared item.
; So in the final answer, we will have
; a very deeply nested list and the square of the last item.
; Instead of a list with just squared integers.
(define (square-list-correct-order items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))

(module+ test
  (square-list (list 1 2 3 4))
  (square-list-correct-order (list 1 2 3 4)))