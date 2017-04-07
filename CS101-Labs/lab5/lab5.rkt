;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname lab5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CMPU-101
; Fall 2016
; Norman Kuang
; Lab 5 - Add it to the list(s)
; Solution

; Data Definition

; A list-of-number (LON) is either
;   - '()
;   - (cons num LON)

; Template

; fun-for-lon : lon -> ...
; template for a function that consumes a lon
(define (fun-for-lon lon)
  (cond
    [(empty? lon) ...]
    [(cons? lon) (... (first lon) ...
                  ... (fun-for-lon (rest lon)) ...)]))

; ----------------------------------------

; sort : LON -> LON
; Sort the numbers in lon, returning a list
; of the same numbers, but in ascending order.
(define (sort lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon) (insert (first lon)
                 (sort (rest lon)))]))

; Uncomment when function ready to test
(check-expect (sort '()) '())
(check-expect (sort (list 42)) (list 42))
(check-expect (sort (cons 7 (cons 2 '())))
              (cons 2 (cons 7 '())))
(check-expect (sort (list 2 7)) (list 2 7))
(check-expect (sort (list 10 5 7 5 2 12 1)) (list 1 2 5 5 7 10 12))

; insert : num LON[sorted] -> LON[sorted]
; Insert n into the right place in a sorted (ascending) list
; to get a list with n and everything already in lon
(define (insert n lon)
  (cond
    [(empty? lon) (cons n '())]
    [(cons? lon)
     (cond
       [(> n (first lon)) (cons (first lon) (insert n (rest lon)))]
       [else (cons n lon)])]))

; Uncomment when function ready to test
(check-expect (insert 5 '()) (cons 5 '()))
(check-expect (insert 5 (cons 6 '())) (list 5 6))
(check-expect (insert 5 (cons 2 '())) (list 2 5))
(check-expect (insert 5 (list 0 2 2 3 5 6 7 8))
                        (list 0 2 2 3 5 5 6 7 8))

; ----------------------------------------

; append-lists : list-of-num list-of-num -> list-of-num
; template for a function with two complex arguments where
; one argument is "along for the ride."

#|
(define (append-lists lon1 lon2)
  (cond
    [(empty? lon1) ... lon2 ... ]
    [(cons? lon1) ... lon2 ...
                  ... (first lon1) ...
                  ... (append-lists (rest lon1) lon2) ...]))
|#


; append-lists : list-of-num list-of-num -> list-of-num
; Consumes two lists, lon1 and lon2, and produces a list with all the
; numbers from lon1 followed by all the numbers from lon2.
(define (append-lists lon1 lon2)
  (cond 
    [(empty? lon1) lon2]
    [(cons? lon1) (cons (first lon1)
                  (append-lists (rest lon1) lon2))]))


; Uncomment when function ready to test
(check-expect (append-lists '() '()) '())
(check-expect (append-lists '() (cons 2 '()))
              (cons 2 '()))
(check-expect (append-lists (cons 2 '()) '())
              (cons 2 '()))
(check-expect (append-lists (cons 2 '()) (cons 3 '()))
              (cons 2 (cons 3 '())))
(check-expect (append-lists (list 2 4 6 8) (list 1 3 5 7))
              (list 2 4 6 8 1 3 5 7))

;; ----------------------------------------

#|
; parallel-sum : LON LON[same length] -> LON[same length]
; template for a function with two complex arguments of
; exactly the same shape, so use essentially the one-argument template.
(define (parallel-sum lon1 lon2)
  (cond
    [(empty? lon1) ... ...]
    [(cons? lon1) ... (first lon1) ... 
                  ... (first lon2) ...
                  ... (parallel-sum (rest lon1) (rest lon2)) ...]))
|#

; parallel-sum : LON LON[same length] -> LON[same length]
; Consumes two lists of same length, lon1 and lon2, and produces
; a list of pairwise sums from lon1 and lon2.
(define (parallel-sum lon1 lon2)
  (cond
    [(empty? lon1) lon2]
    [(cons? lon1) (cons (+ (first lon1) (first lon2))
                  (parallel-sum (rest lon1) (rest lon2)))]))

; Uncomment when function ready to test
(check-expect (parallel-sum '() '()) '())
(check-expect (parallel-sum (cons 3 '()) (cons 2 '()))
              (cons 5 '()))
(check-expect (parallel-sum (list 2 4 6 8) (list 1 3 5 7))
              (list 3 7 11 15))

;; ----------------------------------------

#|
; merge-lists : LON[sorted] LON[sorted] -> LON[sorted]
; template for a function with two complex arguments where you have to
; consider all possible combinations
(define (merge-lists lon1 lon2)
  (cond
    [(and (empty? lon1) (empty? lon2)) ...]
    [(and (empty? lon1) (cons? lon2)) ...
     ... (first lon2) ... (merge-lists '() (rest lon2)) ...]
    [(and (cons? lon1) (empty? lon2)) 
     ... (first lon1) ... (merge-lists (rest lon1) '()) ...]
    [(and (cons? lon1) (cons? lon2))
                  ... (first lon1) ...
                  ... (first lon2) ...
                  ... (merge-lists (rest lon1) lon2) ...
                  ... (merge-lists lon1 (rest lon2)) ...
                  ... (merge-lists (rest lon1) (rest lon2)) ....]))

|#

; merge-lists : LON[sorted] LON[sorted] -> LON[sorted]
; Consumes two sorted lists, lon1 and lon2, and produces a sorted list
; containing all the elements from lon1 and lon2.
(define (merge-lists lon1 lon2)
  (cond
    [(and (empty? lon1) (empty? lon2)) '()]
    [(and (empty? lon1) (cons? lon2)) lon2]
    [(and (cons? lon1) (empty? lon2)) lon1]
    [(and (cons? lon1) (cons? lon2))
      (cond
        [(> (first lon1) (first lon2))
         (cons (first lon2) (merge-lists lon1 (rest lon2)))]
        [else (cons (first lon1) (merge-lists (rest lon1) lon2))])]))


            

; Uncomment when function ready to test
(check-expect (merge-lists '() '()) '())
(check-expect (merge-lists '() (cons 2 '()))
              (cons 2 '()))
(check-expect (merge-lists (cons 2 '()) '())
              (cons 2 '()))
(check-expect (merge-lists (cons 2 '()) (cons 3 '()))
              (cons 2 (cons 3 '())))
(check-expect (merge-lists (cons 3 '()) (cons 2 '()))
              (cons 2 (cons 3 '())))
(check-expect (merge-lists (list 2 4 5 6 8) (list 1 3 5 7))
              (list 1 2 3 4 5 5 6 7 8))

(sort (list 10 5 7 5 2 12 1))