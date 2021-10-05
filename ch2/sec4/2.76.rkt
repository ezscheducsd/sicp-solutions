#lang racket

; Three strategies
; - generic operations with explicit dispatch
; - data-directed style (table)
; - message-passing

; Generic operations
; If new types are added, then we will need to add a new
; conditional to each generic operation (since they
; dispatch on object type) - MODIFY EXISTING CODE
; If new operations are added, we will need to add a new
; generic operations and define its rules for the existing types.
; - ADD NEW CODE

; Data-directed-style
; If new types are added, then we will need to implement
; the existing generic procedures for the new
; type, then install them into the system, associating
; them with the new type (ADD NEW CODE)
; If new procedures are added, then we will need to go into the
; installation code for each type, write a new procedure,
; then register it with the current type (ADD NEW CODE)

; Message-passing
; If new types are added, then we will need to define a new object
; and write conditionals that dispatch on the existing operations
; (ADD CODE)
; If new operations are added, then we will need to
; add a conditional in each existing object (MODIFY EXISTING CODE)

; In a system where new types are often added, then we can use
; either the data directed style or message-passing
; In each case, we are adding new code in only one place.

; In a system where new operations are often added
; Then we can use either generic operations or data directed
; style. In the case of generic operations, we are only adding a
; new operation in one place, but with data-directed, we will
; have to add a new procedure in each of the types' installation code.
; However, data-directed may still be more preferable because
; it is still more flexible to adding new types than generic operations.