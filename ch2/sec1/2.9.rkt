#lang racket

; Width: half the difference b/t upper and lower bound
; Width is a measure of uncertainty.

; For some arithmetic operations, the width of the result
; of combining two intervals is only a function of the widths
; of the argument intervals, whereas for othersow a
; the width of the combination is not a function of the widths
; of the argumument intervals.


; Show that the width of a sum/difference:
; only a function of the widths of the two intervals
; being added (or subtracted)

; Width of the sum of two intervals:
; The result of adding two intervals is:
; (lower1 + lower2, upper1 + upper2)
; The width of this is
; (upper1 + upper2 - lower1 - lower2) / 2
; = (upper1 - lower1) / 2 + (upper2 - lower2) / 2
; First part of this sum is width of first interval.
; Second part of this sum is width of the second interval.

; Width of the difference of two intervals:
; The result of subtracting an interval from another is:
; (lower1 - upper2, upper1 - lower2)
; The width of this is
; (upper1 - lower2 - lower1 + upper2) / 2
; which is the same as for addition.


; How about multiplication?
; Consider these multiplications:
; (-3, 3) * (2, 8) = (-24, 24)
; width 3 * width 3 = width 24
; (2, 8) * (2, 8) = (4, 64)
; width 3 * width 3 = width 30
; Same widths for inputs, but different widths for outputs


; Division?
; The widths of (13, 15) / (2, 4)
; and (8, 10) / (2, 4) are different even though the input
; widths are all the same - all input widths are one.