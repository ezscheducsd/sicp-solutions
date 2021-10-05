#lang racket

; internal procedures in scheme-number
; from generic package are nothing more than calls to primitive procedures
; However, we cannot use primitive with the generic system
; because they require type tags.

; Consider lisp's primitive predicates symbol? and number?
; which determine whether data objects have particular types.
; modify type-tag, contents, and attach-tag
; so that our generic system takes advantage of Scheme's internal type system.

; The changes we made can be seen in 2.4.2-tagged-data:
; when tagging a primitive, we don't do anything.
; when getting the tag for a primitive, it is 'scheme-number
; when getting the contents for a primitive, we just return the number itself

; See changes to 2.4.

(require "2.5.1-generic-arithmetic.rkt")

(module+ test
  (add 1 2)
  )