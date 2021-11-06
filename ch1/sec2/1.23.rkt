#lang racket

(require "prime.rkt")
(require sicp-helpers/mathlib)
(require "1.22.rkt")

(define (prime-sd-fast? n)
  (= n (smallest-divisor-fast n)))

(define (next x)
  (if (= x 2) 3 (+ x 1)))

(define (smallest-divisor-fast x)
  (define (helper cur)
    (cond ((> (square cur) x) x)
          ((= 0 (remainder x cur)) cur)
          (else (helper (next cur)))))
  (helper 2))

(define (start-prime-test n start-time)
  (when (prime-sd-fast? n)
      (report-time (- (current-inexact-milliseconds) start-time))))

(define (report-time elapsed-time)
  (display "***")
  (display elapsed-time))


(define (timed-prime-test-fast n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (time-n-times f n)
  (define start-time (current-inexact-milliseconds))
  (define (helper times)
    (if (= times 0)
        (display "Done!")
        (begin
          (f)
          (helper (- times 1)))))
  (display "Done!!!:")
  (newline)
  (display (- (current-inexact-milliseconds) start-time)))

(module+ test
  (time-n-times
   (lambda ()
     (timed-prime-test 1000000007))
   1000)
  
  (time-n-times
   (lambda ()
     (timed-prime-test 1000000007))
   1000)
  
  )

;1000000007***1.395751953125
;1000000007***0.615478515625
; We can see that the prime test