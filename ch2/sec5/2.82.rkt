#lang racket

; Show how to generalize apply-generic
; to handle coercion in the case of multiple arguments

; Try to coerce all arguments to the first type, to the second type, and so on.
; If we can't coerce to one type all the other types, then we fail.