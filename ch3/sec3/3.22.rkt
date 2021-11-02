#lang sicp

; Build a queue as a procedure with local state.
; Local state: pointers to beginning and end of
; an ordinary list.

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    
    (define (front-queue)
      (if (empty-queue?)
          (error "Cannot get the front of an empty queue.")
          (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-item (cons item '())))
        (cond
          ((empty-queue?)
           (set! front-ptr new-item)
           (set! rear-ptr new-item))
          (else
           (set-cdr! rear-ptr new-item)
           (set! rear-ptr new-item)))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             ; Cannot delete from an empty queue.
             (error "DELETE! called with an empty queue"))
            ; simply set the front ptr of the queue to
            ; the cdr of the current front ptr
            (else
             (let ((item (car front-ptr)))
               (set! front-ptr (cdr front-ptr))
               item))))

    (define (print-queue)
      (display "Queue contains: ")
      (newline)
      (display front-ptr)
      (newline))
    
    (define (dispatch m)
      (cond
        ((eq? m 'empty?) (empty-queue?))
        ((eq? m 'front) (front-queue))
        ((eq? m 'insert!) insert-queue!)
        ((eq? m 'delete!) (delete-queue!))
        ((eq? m 'print) (print-queue))
        (else
         (error "Unrecognized queue dispatch message: " m))))
    dispatch))

(#%provide make-queue)