#lang racket

; Consider the encoding procedure of 2.68.

; Order of growth in the number of steps needed to encode a symbol?

; Consider the special case where the relative frequencies of the n
; symbols are as described in Exercise 2.71.
; Be sure to include the number of steps needed to search
; the symbol list at each node encountered

; In the case of 2.71
; The tree will have a depth of n-1
; The least frequent symbol (freq. 1) will be a leaf
; at a depth of n-1.
; The most frequent symbol (freq. 2^n-1) will be a leaf
; at a depth of 1.

; Symbols with frequency 1, 2, 4, ..., 2^n-1

; Encode frequency 1 (2^0)
; about n set searches, once - n

; Encode frequency 2 (2^1)
; about 2n - 1 set searches, twice - about 4n

; Encode frequency 4 (2^2)
; about 3n - 2 set searches, 4 times - about 12n

; Encode frequency 8: (2^3)
; about 4n - 3 set searches, 8 times - about 32n

; Encode frequency (2^n-1)
; about n^2 - (n - 1) searches, 2^(n-1) times - about (n^2)(2^(n-1))

; Worst case - descend n-1 levels
; At each node, we will have to search
; The unordered set search will take
; n, n-1, n-2, ... until the last level
; where only 2 are needed (node with two leaves)
; n + n-1 + n-2 + ...  + 2 ~ n^2 searches for a symbol.