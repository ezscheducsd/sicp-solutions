#lang racket

; Huffman tree for an alphabet of n symbols
; Relative frequencies: 1, 2, 4, 2^n-1

; For n = 5: frequencies are
; 1, 2, 4, 8, 16
; Call these L1, L2, L4, L8, L16

; Tree construction for n = 5.
; Combine leaves L1 and L2 to get T3(L1, L2) - depth 1
; Combine T3 and L4 to get T7=(T3, L4) - depth 2
; Combine T7 and L8 to get T15 = (T7, L8) - depth 3
; Finally, combine T15 and L16 to get
; T31(T15, L16) - depth 4.
; How many bits are required to encode the most frequent symbol
; - only 1 bit.
; Least frequent symbol - 4 bits

; In general, for n symbols, the most frequent symbol
; will require only 1 bit.
; The least frequent symbol will require lg(2^(n-1))

; For example, for n = 5, the least frequent symbol will have
; lg(2^4) = 4 bits