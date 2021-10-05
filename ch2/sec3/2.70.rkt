#lang racket

(require "2.3.4-huffman-trees.rkt")
(require "2.69.rkt")
(require "2.68.rkt")

(module+ test
  (define pairs '((a 2) (Get 2) (Sha 3) (Wah 1) (boom 1) (job 2) (na 16) (yip 9)))
  (define sample-tree (generate-huffman-tree pairs))
  (define message '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))
  (define encoding (encode message sample-tree))
  (length encoding)
  ; 84 bits are required for the encoding.
  ; If we used a fixed-length code, we would need 3-bits per symbol (8 symbols)
  ; In total there are 36 words
  ; Which means with a fixed-length encoding, we would need
  ; 36 x 3 = 108 bits
  )