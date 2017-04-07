;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CS101 Lab 2
; Name: Norman Kuang
 
; Exercise 1:
; Define the function letter-grade that consumes a number between 
; 0 and 100, and produces a letter grade ("A", "B", "C", "D", "F") 
; using the usual 90, 80, 70, 60 cutoffs.

;letter-grade : num -> string
;Converts a number n between 0 and 100 and produces a letter grade
;  at the usual cutoffs.
(define (letter-grade n)
  (cond
    [(>= n 90) "A"]
    [(>= n 80) "B"]
    [(>= n 70) "C"]
    [(>= n 60) "D"]
    [(< n 60) "F"]))
(check-expect (letter-grade 100) "A")
(check-expect (letter-grade 90) "A")
(check-expect (letter-grade 80) "B")
(check-expect (letter-grade 70) "C")
(check-expect (letter-grade 60) "D")
(check-expect (letter-grade 55) "F")

; Exercise 2:
; Define the function pass? that determines whether the given number
; grade is a passing grade (60 or higher).

;pass? : num -> string
;Consumes a number n and determines whether the given number grade is
; a passing grade.
(define (pass? n)
  (cond
    [(>= n 60) true]
    [else false]))
(check-expect (pass? 100) true)
(check-expect (pass? 59) false)

; Exercise 3:
; Define the function pass-fail that consumes a number grade and 
; determines either "pass" or "fail". Use the pass? predicate function
; from Exercise 2.

;pass-fail : num -> string
;Consumes a number grade n and determines either "pass" or "fail".
(define (pass-fail n)
  (cond
    [(pass? n) "pass"]
    [else "fail"]))
    
(check-expect (pass-fail 90) "pass")
(check-expect (pass-fail 59) "fail")
(check-expect (pass-fail 60) "pass")

; Exercise 4:
; Define the function NRO-grade, which consumes a number grade and a 
; minimum numeric grade between 0 and 100, and produces either a 
; letter grade, "PA", or "F", according to Vassar's NRO rules. You
; are encouraged to use one or more functions from Exercises 1-3 as

;NRO-grade: num num -> string
;Consumes both a number grade n and a minimum numeric grade (0-100)m
; and produces either a letter grade, "PA", or "F".
(define (NRO-grade n m)
  (cond
    [(>= n m) (letter-grade n)]
    [(< n m) (pass-fail n)]))
    
(check-expect (NRO-grade 99 90) "A")
(check-expect (NRO-grade 89 90) "pass")
(check-expect (NRO-grade 59 60) "fail")
(check-expect (NRO-grade 79 80) "pass")
(check-expect (NRO-grade 85 80) "B")