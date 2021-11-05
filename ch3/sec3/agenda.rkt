#lang sicp

; Used for the queue on time segments
(#%require "queue.rkt")
; Overall structure of the agenda.
(#%require "table.rkt")

(#%provide
 make-agenda
 add-to-agenda!
 current-time
 empty-agenda?
 first-agenda-item
 remove-first-agenda-item!)

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))


; The agenda is implemented as a 1-d table of time segments
; Segments will be sorted in order of increasing time
; Store the curren time at the head of the agenda.
; new agendas have a current time of 0.

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

; empty agenda: no time segments
(define (empty-agenda? agenda)
  (null? (segments agenda)))


(define (add-to-agenda! time action agenda)
  ; If the time for the new action
  ; is earlier than all else, should be
  ; placed in a new time segment at the first position
  ; in the table.
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  ; make a new time segment with the time and action
  ; initialize a queue, associate the queue with the given time.
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  ; Finding a place to add a new time segment.
  (define (add-to-segments! segments)
    ; If the time of the new action equals the time
    ; of the current segment, add it there.
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        ; Otherwise, we will need to insert the new
        ; action into somewhere in the rest of the segments.
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              ; The time segment is greater than the current
              ; but less than all of the rest.
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              ; The time segment is greater than the current
              ; and not less than the first of the rest.
              ; Which means it would go into the rest
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        ; If the new time/action is earlier than
        ; all the rest of the segments
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        ; If not earlier than all the rest,
        ; add into the segments.
        (add-to-segments! segments))))

; Remove the first item from the agenda.
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    ; If the time segment queue is not empty, we set the
    ; segments of the agenda to the rest of the segments
    ; We no longer need to first.
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

; First agenda item is found at the head of the queue
; in the first time segment.
; Whenever we extract an item, we also update the current time
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        ; When getting the first item
        ; we set the 
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))