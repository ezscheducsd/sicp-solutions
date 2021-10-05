#lang racket

(module+ test
  (define l1 (list 1 3 (list 5 7) 9))
  ; select 7 from the following lists.

  ;(cdr l1) ;
  ;; get the list (3 (5 7) 9)
  ;(cdr (cdr l1))
  ;; get the list ((5 7) 9)
  ;(car (cdr (cdr l1)))
  ;; get the list (5 7)
  ;(cdr (car (cdr (cdr l1))))
  ;; get the list (7)
  ;(car (cdr (car (cdr (cdr l1)))))
  ;; get 7.
  
  (define l2 (list (list 7)))
  ; (car l2)
  ; get the list (7)
  ; (car (car l2))
  ; get 7.

  (define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
  ; (cdr l3)
  ; get the list containing (2 (3 (4 (5 (6 7)))))
  ; (car (cdr l3))
  ; get the list (2 (3 (4 (5 (6 7)))))

  (require rackunit)
  (check-eq? (car (cdr (car (cdr (cdr l1)))))
             7)
  (check-eq? (car (car l2))
             7)
  (check-eq? (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))))
             7)
  
  )