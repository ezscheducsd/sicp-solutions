#lang sicp

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

; implement actual queue operations
(define (empty-queue? queue)
  (null? (front-ptr queue)))

; Constructor - front ptr and rear ptr are
; both simply empty lists
(define (make-queue) (cons '() '()))

; If queue is empty, we have an error
; Otherwise just select the first element
; in the queue (which is represented as a list.)
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

; Insert queue
(define (insert-queue! queue item)
  ; Create a new pair representing the new item
  (let ((new-pair (cons item '())))
    ; If the queue is empty, both queue pointers
    ; should point to this new queue
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          ; Otherwise, make the current rear's
          ; cdr point to the new item,
          ; then set the rear ptr to the new item
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

; Delete queue
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         ; Cannot delete from an empty queue.
         (error "DELETE! called with an empty queue" queue))
        ; simply set the front ptr of the queue to
        ; the cdr of the current front ptr
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

; Exericse 3.21: print-queue
(define (print-queue queue)
  ; We can simply print the front ptr.
  (display (front-ptr queue))
  (newline))

(#%provide make-queue insert-queue!
           front-queue delete-queue!
           empty-queue?
           print-queue)
