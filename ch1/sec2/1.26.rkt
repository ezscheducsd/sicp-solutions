#lang racket

;Exercise 1.26: Louis Reasoner is having great difficulty do-
;ing Exercise 1.24. His fast-prime? test seems to run more
;slowly than his prime? test. Louis calls his friend Eva Lu
;Ator over to help. When they examine Louis’s code, they
;find that he has rewritten the expmod procedure to use an
;explicit multiplication, rather than calling square :

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base
                       (expmod base (- exp 1) m))
                    m))))

; So we know that the old expmod runs in log n time.

; Why does this not run in log n time as well?
; Well, we can see that we are evaluating
; (expmod base (/ exp 2)) twice in each recursive call.
; The depth of the process will still be log(n),
; but we will be branching out for each of the two
; recursive calls.
; So there will be a lot of redundant computation.

; Remember, tree recursion takes time about the
; amount of nodes in the recursion tree.
; At the bottom level (which is log n deep)
; we would need to do 2^log n steps.
; A little bit rusty on the math though...
