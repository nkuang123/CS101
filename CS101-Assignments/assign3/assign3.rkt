;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname assign3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;------------------------------------------------------------------------
; CMPU-101 Assignment 3 
; Fall 2016
; Norman Kuang
; Description: Permutations
;   Find all the permutations of a given list of numbers
;------------------------------------------------------------------------

;------------------------------------------------------------------------
; Data Definitions
;------------------------------------------------------------------------

; A list-of-num (LON) is either
; - '()
; - (cons num LON)

; A list-of-LON (LOLON) is either
; - '()
; - (cons LON LOLON)



; permutations: LON -> LOLON
; to create a list of all permutations of the numbers in lon
(define (permutations lon)
  (cond
    [(empty? lon) (list '())]
    [(cons? lon) (insert-everywhere/in-all-lons (first lon)
                                                (permutations (rest lon)))]))

(check-expect (permutations '()) (list '()))
(check-expect (permutations '(1)) (list '(1)))
(check-expect (permutations '(1 2)) (list '(1 2) '(2 1)))
(check-expect (permutations '(1 2 3)) (list '(1 2 3) '(2 1 3) '(2 3 1)
                                            '(1 3 2) '(3 1 2) '(3 2 1)))


; You continue designing/implementing/testing the following functions,
; in order. You should not need any additional functions.


; insert-everywhere/in-all-lons: num LOLON -> LOLON
; Inserts given number at the beginning, between all numbers, and at the end 
; of all LONs in the given list of lists of numbers.
(define (insert-everywhere/in-all-lons n lolon)
  (cond
    [(empty? lolon) '()]
    [else (append (insert-everywhere/in-one-lon n (first lolon))
                  (insert-everywhere/in-all-lons n (rest lolon)))]))

(check-expect (insert-everywhere/in-all-lons 1 (list (list 2 3) (list 4 5)))
              (list '(1 2 3) '(2 1 3) '(2 3 1) '(1 4 5) '(4 1 5) '(4 5 1)))
(check-expect (insert-everywhere/in-all-lons 1 (list '()))
              (list (list 1)))


; insert-everywhere/in-one-lon: num LON -> LOLON
; Returns a list of numbers with the given number inserted in front of, 
; in between, and at the end of each number in the given list of numbers.
(define (insert-everywhere/in-one-lon n lon)
  (cond
    [(empty? lon) (list (list n))]
    [else (cons (cons n lon)
                (insert-beginning/of-all-lons (first lon)
                (insert-everywhere/in-one-lon n (rest lon))))]))
               
(check-expect (insert-everywhere/in-one-lon 1 (list 2 3))
              (list '(1 2 3) '(2 1 3) '(2 3 1)))
(check-expect (insert-everywhere/in-one-lon 1 '())
              (list (list 1)))
   

; insert-beginning/of-all-lons: num LOLON -> LOLON
; Inserts the given number at the beginning of each LON in given list of LONs.
(define (insert-beginning/of-all-lons n lolon)
  (cond
    [(empty? lolon) '()]
    [else (cons (cons n (first lolon))
                (insert-beginning/of-all-lons n (rest lolon)))]))

(check-expect (insert-beginning/of-all-lons 1 (list (list 2 3) (list 3 4)))
              (list (list 1 2 3) (list 1 3 4)))
(check-expect (insert-beginning/of-all-lons 1 '()) '())





