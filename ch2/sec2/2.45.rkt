#lang racket

;(define (right-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (right-split painter (- n 1))))
;        (beside painter (below smaller smaller)))))
;
;(define (up-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (up-split painter (- n 1))))
;        (below painter (beside smaller smaller)))))

;(define right-split (split beside below))
;(define up-split (split below beside))

; Split higher-order operation:
; The second argument is what is applied to
; two instances of the split painter (at n-1)
; The first argument is what is applied to
; the identity and the combined split.
(define (split t1 t2)
  (define (helper painter n)
    (if (= n 0)
        painter
        (let ((smaller (helper painter (- n 1))))
          (t1 painter (t2 smaller smaller)))))
  helper)