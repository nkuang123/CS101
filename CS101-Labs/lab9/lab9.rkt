;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CMPU-101
; Fall 2016
; December 7, 2016
; Norman Kuang

; Use build-list and lambda to define the four
;  functions that, respectively:

; Exercise 1)
; creates the list (list 0 ... (- n 1)) for any natural number n

; build-list1 : NN -> list-of-NN
; creates the list '(0 ... (- n 1)) for any NN n
(define (build-list1 n)
  (build-list n (lambda (m) m)))

(check-expect (build-list1 1) '(0))
(check-expect (build-list1 5) '(0 1 2 3 4))

; Exercise 2)
; creates the list (list 1 ... n)) for any natural number n

; build-list2: NN -> list-of-NN
; creates the list '(0 ... n) for any natural number n
(define (build-list2 n)
  (build-list n (lambda (m) (add1 m)))) 

(check-expect (build-list2 1) '(1))
(check-expect (build-list2 5) '(1 2 3 4 5))

; Exercise 3)
; creates the list (list 1 1/2 ... 1/n) for any natural number n

; build-list3: NN -> list-of-NN
; creates the list '(1 1/2 ... 1/n) for any natural number n
(define (build-list3 n)
  (build-list n (lambda (m) (/ 1 (add1 m)))))

(check-expect (build-list3 1) '(1))
(check-expect (build-list3 5) '(1 1/2 1/3 1/4 1/5))

; Exercise 4)
; creates the list of the first n even numbers

; build-list4: NN -> list-of-NN
; creates the list of the first n even numbers
(define (build-list4 n)
  (build-list n (lambda (m) (* 2 m))))

(check-expect (build-list4 0) '())
(check-expect (build-list4 6) '(0 2 4 6 8 10))

; Exercise 5)
; use foldr to define append-from-fold

; append-from-fold : list-of-numbers list-of-numbers -> list-of-numbers
; Uses foldr to combine two lists of numbers lon1, lon2
;  into one list of numbers
(define (append-from-fold lon1 lon2)
  (foldr cons lon2 lon1))

(check-expect (append-from-fold '(1 2 3 4) '(5 6 7 8)) '(1 2 3 4 5 6 7 8))
(check-expect (append-from-fold '() '(1 2 3)) '(1 2 3))

; Exercise 6)
; use foldr to define map-via-fold

; map-via-fold : (X -> X) list-of-X -> list-of-X
; Maps the given function x onto elements of lox
(define (map-via-fold f lox)
  (foldr (lambda (i r) (cons (f i) r)) '() lox))

(check-expect (map-via-fold add1 '(1 2 3 4)) '(2 3 4 5))



















 
