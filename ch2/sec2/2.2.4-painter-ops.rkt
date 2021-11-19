#lang racket
(require sicp-pict)

(define ein einstein)

; Our primitive means of combination are
; simple operations like flip-horiz, flip-vert
; , beside, below, rotate90, etc.

; Simple abstraction of a typical combination
;(define (flipped-pairs painter)
;  (let ((painter2 (beside painter (flip-vert painter))))
;    (below painter2 painter2)))
; Flipped-painter using the higher-order square-of-four
(define (flipped-pairs painter)
  ((square-of-four identity flip-vert identity flip-vert) painter))

; Exercise 2.45: Right-split and up-split
; expressed as instances of a general splitting operation
; The first procedure is how the painter is combined with the
; recursive painter.
; The second procedure is how the two recursive painters
; are combined
(define (split combine-with-rec combine-recs)
  (define (helper painter n)
    (if (= n 0)
        painter
        (let ((smaller (helper painter (- n 1))))
          (combine-with-rec painter (combine-recs smaller smaller)))))
  helper)

; Recursive operations
;(define (right-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (right-split painter (- n 1))))
;        (beside painter (below smaller smaller)))))
(define right-split (split beside below))

; Ex. 2.44: up-split
;(define (up-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (up-split painter (- n 1))))
;        (below painter (beside smaller smaller)))))
; Higher-order up-split using general split
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

; Use corner-split on each corner of the square
;(define (square-limit painter n)
;  (let ((quarter (corner-split painter n)))
;    (let ((half (beside (flip-horiz quarter) quarter)))
;      (below (flip-vert half) half))))
; Square-limit using the higher order square-of-four
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

; HIGHER-ORDER operations
; example: flipped-pairs and square-imit each arrange four copies
; of a painter's image in a square pattern.
; differ only in how they orient the copies

; Higher order procedure that takes
; painter operations and procedures another painter operation
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))


(module+ test
  (paint (square-limit ein 3))
  )