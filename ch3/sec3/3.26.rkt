#lang sicp

; Describe a table implementation where
; records are organized using a binary tree.
; Compare 2.66 exercise.

; We will need to implement the procedures
; make-table
; lookup and assoc
; insert!

; So the overall idea is that we will have binary search
; tree structure ordered according to the keys
; that are inserted into the table.

; Each record will be a record consisting of a key and a value
; as before.
; The lookup and adjoin-set procedures for the binary tree
; are currently implemented where direct values are inserted and lookedup
; instead of key-value pairs.

; So we will need to change those procedures to work with
; key-value records, where records are compared according to key
; using some keycomparator.
; so lookup-tree takes in a key as an argument, compares that
; to the key of the current root record
; insert-tree takes a key and a value.
; compares the key to the key of the current root record
; either create a new tree structure or update the record that has
; the matching key.