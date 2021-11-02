#lang racket

(require compatibility/mlist)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (mcdr x)))
          (set-mcdr! x y)
          (loop temp x))))
  (loop x '()))

; in general, mystery reverses the list x.

(module+ test
  (define v (mlist 'a 'b 'c 'd))
  (define w (mystery v))

  ; after tracing out the execution, w should be the reversed list.
  ; v should now just point to the list containing the single item, a
  w
  v
  )