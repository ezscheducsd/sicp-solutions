#lang racket

; a.
; implement for headquarters a get-record procedure that
; retrieves a specified employee's record from a personnel file

; Example division: california division.

; a.
; HQ is given an employee name and a personnel file
(define (get-record employee-name personnel-file)
  ; personnel files should be tagged with the division name
  ; for hq to use
  (let ((division (get-division personnel-file)))
    ((get 'employee-lookup division) employee-name personnel-file)))

; b.
; get-salary: returns salary information from a given employee's record
; from any division's personnel file
; The record should be tagged with the division name as well
(define (get-salary employee-record)
  (let ((division (get-division employee-record)))
    ((get 'get-salary division) employee-record)))

; c.
; find-employee-record
; given an employee name, find the employee record.
; Search from all divisions
; we are given employee name and division's files as a list

; Idea:
; no more files - not found
; search the current file
; if found, return
; if not found, search the rest of the files
(define (find-employee-record employee-name personnel-files)
  (if (null? personnel-files)
      #f
      (let* ((current-file
              (car personnel-files))
             (division
              (get-division current-file))
             (employee-or-false
              ((get 'employee-lookup division) employee-name current-file)))
        (if (false? employee-or-false)
            (find-employee-record employee-name (cdr personnel-files))
            employee-or-false))))

; d.
; When taking over a new company, what changes need to be made in order to incorporate
; the new personnel information into the central system.
; Suppose that the company has its own divisions
; and its own employee record structure
; In order to accommodate the new changes,
; The company needs to provide its personnel record
; and install its own employee-lookup
; and get-salary procedures
; Along with any future procedures that might be needed.