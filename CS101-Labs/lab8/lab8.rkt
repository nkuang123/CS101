;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CMPU-101
; Fall 2016
; Lab #8
; Norman Kuang

(define-struct toast (kind level))
; A toast is a
;   (make-toast string number[0 to 10])
; where where a toast’s kind can be any string,
;  including (but not limited to) “white” or “wheat”. 

; A list-of-toast (lot) is either
; - '()
; - (cons toast lot)

(define toast-list
  (list (make-toast "white" 0)
        (make-toast "wheat" 0)
        (make-toast "white" 4)
        (make-toast "wheat" 4)
        (make-toast "white" 6)
        (make-toast "wheat" 6)
        (make-toast "white" 10)
        (make-toast "wheat" 10))) ; Example of a list-of-toast

; Exercise 1:
; Implement the function count-white, which
;  takes a list-of-toast and returns the number of toasts
;  in the list with the kind "white".

; count-white: list-of-toast -> number
; returns the number of toasts in the list with the kind "white"
(define (count-white lot)
  (cond
    [(empty? lot) 0]
    [(cons? lot)
     (local [(define count-of-rest (count-white (rest lot)))]
     (cond
       [(string=? (toast-kind (first lot)) "white") 
                  (add1 count-of-rest)]
       [else count-of-rest]))]))

(check-expect (count-white '()) 0)
(check-expect (count-white toast-list) 4)

; Exercise 2:
; Implement the function count-wheat, which takes a list-of-toast
;  and returns the number of toasts in the list with the kind “wheat”.

; count-toast: list-of-toast string -> number
; returns the number of toasts in the list with the given kind
(define (count-toast lot kind)
  (cond
    [(empty? lot) 0]
    [(cons? lot)
     (local [(define count-of-rest (count-toast (rest lot) kind))]
     (cond
       [(string=? (toast-kind (first lot)) kind) 
                  (add1 count-of-rest)]
       [else count-of-rest]))]))

;; count-wheat: list-of-toast -> number
;; returns the number of toasts in the list with the kind "wheat"
(define (count-wheat lot)
  (count-toast lot "wheat"))

(check-expect (count-wheat '()) 0)
(check-expect (count-wheat toast-list) 4)

; Exercise 3:

; count-bread : list-of-toast (toast -> bool) -> num
; Return the number of toast that satisfies PRED
(define (count-bread lot PRED) (cond
    [(empty? lot) 0]
    [(cons? lot)
     (local [(define count-of-rest (count-bread (rest lot) PRED))]
     (cond
       [(PRED (first lot))
        (add1 count-of-rest)]
       [else count-of-rest]))]))

; count-untoasted : list-of-toast -> num
; Return the number of toast is the list with toast
;  level 0.
(define (count-untoasted lot)
  (local [; untoasted? : toast -> bool
          ; determines whether t is untoasted (toast level 0)
          (define (untoasted? u)
            (= (toast-level u) 0))]
    (count-bread lot untoasted?)))
    

(check-expect (count-untoasted '()) 0)
(check-expect (count-untoasted toast-list) 2)

; Exercise 4:

; count-bread : list-of-toast (toast -> bool) -> num
; Return the number of toast that satisfies PRED
;(define (count-bread lot PRED) (cond
;    [(empty? lot) 0]
;    [(cons? lot)
;     (local [(define count-of-rest (count-bread (rest lot) PRED))]
;     (cond
;       [(PRED (first lot))
;        (add1 count-of-rest)]
;       [else count-of-rest]))]))

; count-yummy : list-of-toast -> num
; Returns the number of toasts in the list-of-toasts
;  that are yummy (toast level 4 - 8)
(define (count-yummy lot)
  (local [; yummy? : toast -> bool
          ; Determines whether wheat toast is yummy (btwn level 4 and 8 inclusive)
          (define (yummy? t)
            (and (string=? (toast-kind t) "wheat")
                 (<= 4 (toast-level t) 8)))]
    (count-bread lot yummy?)))

(check-expect (count-yummy '()) 0)
(check-expect (count-yummy toast-list) 2)

; Exercise 5 (using built-in functions; same as #4, but using
;  built in functions):

; count-yummy2 : list-of-toast -> num
; Returns the number of toasts in the list-of-toasts
;  that are yummy (toast level 4 - 8)
(define (count-yummy2 lot)
  (local [; yummy? : toast -> bool
          ; Determines whether wheat toast is yummy (btwn level 4 and 8 inclusive)
          (define (yummy? t)
            (and (string=? (toast-kind t) "wheat")
                 (<= 4 (toast-level t) 8)))]
    (length (filter yummy? lot))))

(check-expect (count-yummy2 '()) 0)
(check-expect (count-yummy2 toast-list) 2)
















