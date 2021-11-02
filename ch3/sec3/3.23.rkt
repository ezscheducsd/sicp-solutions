#lang sicp

; Deque (double-ended deque)
; sequence in which items can be inserted and deleted at
; either the front or the rear

; operations on deques are
; make-deque constructor
; empty-deque?
; front-deque, rear-deque selectors
; front-insert-deque!
; rear-insert-deque!
; front-delete-deque!
; rear-delete-deque!

; How should we implement this?
; We can start out with the same idea as a regular deque,
; where we have a front ptr and a rear ptr
; similar operations.

; Be aware of the cases where both rear ptr and
; front ptr might have to be modified

; Insert front is the same as regular deque
; Rear-insert is very similar to front insert
; front delete is the same as regular deque

; Rear delete: This one is hard because if
; we are representing the deque as a single list,
; we will need to move to rear ptr
; back one
; BUT.
; Regular lists in scheme don't really have to way
; to move backward in the list.
; So either we:
; - pack each item its rear
; - use two lists to represent the deque
; - be careful with the two lists because
; there might be redundant storage.


; Again, a deque will be represented as a pair of a front ptr and
; a rear ptr
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item)
  (set-car! deque item))
(define (set-rear-ptr! deque item)
  (set-cdr! deque item))
(define (make-deque) (cons '() '()))

; A deque is empty if the front ptr is empty
(define (empty-deque? deque) (null?  (front-ptr deque)))

; deque items will be a triple
; consisting of:
; item (value)
; next-ptr
; prev-ptr
(define (make-item val prev next)
  (cons val (cons prev next)))

; Selectors and mutators
(define (val-item item) (car item))
(define (prev-item item) (cadr item))
(define (next-item item) (cddr item))
(define (set-prev-item! item prev)
  (set-car! (cdr item) prev))
(define (set-next-item! item next)
  (set-cdr! (cdr item) next))

; Deque selectors
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "Cannot select front from an empty deque.")
      (val-item (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "Cannot select rear from an empty deque.")
      (val-item (rear-ptr deque))))

; Insertion at front
(define (front-insert-deque! deque val)
  (cond
    ((empty-deque? deque)
     (let ((new-item (make-item val '() '())))
       (set-front-ptr! deque new-item)
       (set-rear-ptr! deque new-item)))
    ; the current front should point back at the new item
    (else
     (let ((new-item (make-item val '() (front-ptr deque))))
       (set-prev-item! (front-ptr deque) new-item)
       (set-front-ptr! deque new-item)))))

; Removal from the front
(define (front-delete-deque! deque)
  (cond
    ((empty-deque? deque)
     (error "Cannot remove from the front of an empty deque."))
    ; There is only one more node if the next item
    ; of the front is null
    ((null? (next-item (front-ptr deque)))
     (let ((val (val-item (front-ptr deque))))
       (set-front-ptr! deque '())
       (set-rear-ptr! deque '())
       val))
    (else
     (let ((val (val-item (front-ptr deque))))
       (set-front-ptr! deque (next-item (front-ptr deque)))
       ; The new front item now points back at null
       (set-prev-item! (front-ptr deque) '())
       val))))

; insertion at the rear.
; pretty similar to insertion at the front.
(define (rear-insert-deque! deque val)
  (cond
    ((empty-deque? deque)
     (let ((new-item (make-item val '() '())))
       (set-front-ptr! deque new-item)
       (set-rear-ptr! deque new-item)))
    (else
     (let ((new-item (make-item val (rear-ptr deque) '())))
       (set-next-item! (rear-ptr deque) new-item)
       (set-rear-ptr! deque new-item)))))

; removal from the rear.
; pretty similar to removal from the front.
(define (rear-delete-deque! deque)
  (cond
    ((empty-deque? deque)
     (error "Cannot remove from the rear of an empty deque."))
    ; There is only one more node if the previous item
    ; of the rear is null
    ((null? (prev-item (rear-ptr deque)))
     (let ((val (val-item (rear-ptr deque))))
       (set-front-ptr! deque '())
       (set-rear-ptr! deque '())
       val))
    (else
     (let ((val (val-item (rear-ptr deque))))
       (set-rear-ptr! deque (prev-item (rear-ptr deque)))
       ; The new rear item now forward at null
       (set-next-item! (rear-ptr deque) '())
       val))))

(#%provide make-deque
           front-deque
           rear-deque
           front-insert-deque!
           empty-deque?
           front-delete-deque!
           rear-insert-deque!
           rear-delete-deque!
           )