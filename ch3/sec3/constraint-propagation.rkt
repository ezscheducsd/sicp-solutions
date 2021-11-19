#lang racket

; constraint propagation system

; constraints are joined by connectors
; connector is an object that holds a value
; that may participate in one or more constraints

; Constraint system is implemented via procedural
; objects with local state.
; Connector operations
; has-value? - does the connector have a value
; get-value - return the current value of the connector
; set-value! - indicate that the informant is requesting
; the connector to set its value to the new value
; forget-value! tell the connector that the retractor
; is requresting to forget its value
; connect - tells the connector to participate in a new constraint

; connectors communicate with constraints with the procedure
; inform-about-value, inform-about-no-value (when the connector
; has lost a value)

; adder constraint
; constructs an adder constraint among summand connectors
; and a sum connector
(define (adder a1 a2 sum)
  ; when this constraint is informed of a new value
  ; from one of its connectors
  (define (process-new-value)
    ; compute sum from a1 and a2
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ; compute a2 from a1 and sum
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ; compute a1 from a2 and sum
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  ; when this constraint is informed to
  ; forget its values
  ; It is important to note that
  ; only the values that were set by this adder are actually
  ; lost.
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    ; process-new-value is necessary because
    ; one or more connectors may still have a value
    ; (had a value that was not originally set by the adder)
    (process-new-value))
  ; me refers to this constraint.
  ; the constraint is a procedure that, when given a request,
  ; dispatches on the request message.
  ; Either process a new value or forget values
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
  ; Associate the connectors with the constraint
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

; procedures that connectors use to communicate changes to constraints
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

; multipliers are similar to adders.
(define (multiplier m1 m2 product)
  (define (process-new-value)
    ; If either factors is 0, then product is 0
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ; if factors both have a value, compute the
          ; product
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ; If the product has a value and
          ; one factor has a value, we can compute the other factor
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (get-value m2))
                       me))))
  ; similar to adder constraint
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  ; The actual constraint object, which dispatches on request
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: MULTIPLIER"
                       request))))
  ; associate the connectors with the constraint
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

; Constant constraint simply sets the value of the designated connector
; Any request to change the value raises and error.
; After all, this is a constant
(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

; Probe: message about setting or unsetting of a connector
; A probe has similar structure to a
; constraint. However, it is just meant to display messages
; about updates
(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ") (display name)
    (display " = ") (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: PROBE" request))))
  ; Associate the connector with the probe object
  (connect connector me)
  me)

; Connector representation
; represented as a procedural object with local state
; local state is:
; - value - the current value of the connector
; - informant - the object that set the connector's value
; - constraints - the list of constraints in which
; the connector participates (also includes probes?)
(define (make-connector)
  ; initial state
  ; value: false (unset)
  ; informant: false (connector's value has not been set yet
  ; constraint: no constraints have been associated with
  ; this connector yet
  (let ((value false) (informant false) (constraints '()))
    ; internal proc for setting the value of thsi connector
    
    (define (set-my-value newval setter)
      ; If this connector does not yet have a value,
      ; Set the new value, the informant, and
      ; inform each constraint about the new value
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             ; Inform all constraints except for the setter
             (for-each-except setter
                              inform-about-value
                              constraints))
            ; If this co,nnector already has a value and it is different
            ; from the new value being passed in,
            ; inform of a contradiction
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            ; Has a value and new value is the same - don't do anything
            (else 'ignored)))
    ; Internal proc for forgetting the value of this connector
    (define (forget-my-value retractor)
      ; Check that the retractor is the same as the informant
      (if (eq? retractor informant)
          ; If so, unset the informant and
          ; inform each constraint about the unset value.
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    ; Associate this connector with a new constraint
    (define (connect new-constraint)
      (when (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (when (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: CONNECTOR"
                         request))))
    me))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(provide make-connector
         multiplier
         constant
         adder
         probe
         set-value!
         has-value?
         forget-value!
         get-value
         connect)

(module+ test
  ; Test a celsius-fahrenheit system
  (define (celsius-fahrenheit-converter c f)
    (let ((u (make-connector))
          (v (make-connector))
          (w (make-connector))
          (x (make-connector))
          (y (make-connector)))
      (multiplier c w u)
      (multiplier v x u)
      (adder v y f)
      (constant 9 w)
      (constant 5 x)
      (constant 32 y)
      'ok))
  (define C (make-connector))
  (define F (make-connector))
  (celsius-fahrenheit-converter C F)
  (probe "Celsius temp" C)
  (probe "Fahrenheit temp" F)
  (set-value! C 25 'user)
  )