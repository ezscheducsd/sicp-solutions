#lang racket

; Using this procedure, write a procedure search-for-primes
; that checks the primality of consecutive odd integers in a
; specified range. Use your procedure to find the three small-
; est primes larger than 1000; larger than 10,000; larger than
; 100,000; larger than 1,000,000. Note the time needed to test
; each prime. Since the testing algorithm has order of growth
; √
; of Θ( n), you should expect
; √ that testing for primes around
; 10,000 should take about 10 times as long as testing for
; primes around 1000. Do your timing data bear this out?
; How well do the data for 100,000 and 1,000,000 support the
; √
; Θ( n) prediction? Is your result compatible with the notion
; that programs on your machine run in time proportional to
; the number of steps required for the computation?

(require sicp-helpers/mathlib)
(require "prime.rkt")

(provide timed-prime-test)

(define (start-prime-test n start-time)
  (when (prime? n)
      (report-time (- (current-inexact-milliseconds) start-time))))

(define (report-time elapsed-time)
  (display "***")
  (display elapsed-time))


(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))


;Procedure: check the primality of consecutive odd integers in a range.
;Arguments: start, end (noninclusive.)

(define (next x)
  (if (is-even? x) (+ x 1) (+ x 2)))

(define (get-next-n-primes start count)
  (when (> count 0)
    (timed-prime-test start)
    (if (prime? start)
        (get-next-n-primes (next start) (- count 1))
        (get-next-n-primes (next start) count))))

;next 3 primes after 1000: 1009, 1013, 1019
;next 3 primes after 10000: 10007, 10009, 10037
;next 3 primes after 100000: 100003, 100019, 100043

; Sample outputs for near 100.000, 1.000.000
; 1000037***0.018798828125
; 10000019***0.058349609375
; quotient is about 3.1038961039, which is very close
; to sqrt(10) approx. 3.162277

; So clearly, the data very closely support the
; Theta(sqrt(n)) prediction.

; Is your result compatible with the notion that
; programs on your computer run in time proportional to
; the number of steps required for the computation?
; Not sure about this one...
; But it seems like yes? IDK


(define (test-primes-runtime)
  (define (helper start)
    (get-next-n-primes start 3)
    (helper (* start 10)))
  (helper 10))

(module+ test
  (test-primes-runtime))