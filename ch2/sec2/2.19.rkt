#lang racket

; define the undefined procedures
; and does the order of the coin list affect the result?
; why or why not?

(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)


(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

; It looks like the ordering of the coins doesn't
; affect the final answer.

(module+ test
  (define us-coins (list 1 10 25 5 50))
  (cc 100 us-coins))