#lang racket

(require "prime.rkt")
(require "1.22.rkt")

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

(module+ test
  (timed-prime-test-fast 100019)
  (timed-prime-test 100019))