;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CS101 Lab 3
; Name: Norman Kuang

; Exercise 1: List the functions that the following sample
;  definitions define.

(define-struct photo (image tag))
; A photo is a structure:
;     (make-photo Image String)
;     (photo-image)
;     (photo-tag)

(define-struct 3d (x y z))
; A 3d model is (represented by) a structure:
;     (make-3d Number Number Number)
;     (3d-x)
;     (3d-y)
;     (3d-z)
(define-struct ta (last given lab))
; A ta is a structure:
;     (make-ta String Boolean String)
;     (ta-last)
;     (ta-given)
;     (ta-lab)

; Exercise 2: Create at least two data examples per
;  data definition.

(define-struct item (tag price))
; An Item is a structure:
;   (make-item String PositiveNumber)
(make-item "Bubble Gum" 0.99)
(make-item "Lego Set" 4.99)


(define-struct phd (name grant pay-rate))
; A PhD student is (represented by) a structure:
;   (make-phd String GrantId PositiveNumber)
; A GrantId is one of:
; – "1-123"
; – "3-AB4"
; – "9-999"
(define Susie (make-phd "Susie" "99956789" 50000))
(define Jill (make-phd "Jill" "99999999" 95203))


; Exercise 3: The Zoo keeps track of information for every
;  animal that is kept there. For each animal, they store its
;  name, species, age, breakfast hour, and dinner hour (if they
;  don't get fed twice a day, they try to eat the visitors).
; a) Define a structure type Animal for representing information
;  about a zoo animal.

(define-struct Animal (name species age breakfast dinner))
; An animal is a structure
;     (make-Animal String String Number Number Number)

; Exercise 4: Construct a template for functions that process
;  items.

; Consumes an item item and produces a name and a price.
; (define-struct item (tag price)
;   ... (item-tag) ... (item-price)...)
; (define (itemfunc item)
;   ... (item-tag)... (item-price)...)

; Exercise 5: Construct a template for functions that process
;  PhDs.

; Consumes a phd phd and produces a phd student with name, grant, and payrate.
; (define-struct phd (name grant payrate)
;   ... (phd-name)...(phd-grant)...(phd-payrate)...)
; (define (phdfunc phd)
;   ... (phd-name)...(phd-grant)...(phd-payrate)...)


; Exercise 6: Construct a template for functions that process
;  Animals.

; Consumes an animal and produces its name, species, age, breakfast hour, and dinner hour.
; (define-struct Animal (name species age breakfast dinner)
;   ...(Animal-name)...(Animal-species)...(Animal-age)...(Animal-breakfast)...(Animal-dinner)...)
; (define (animalfunc Animal)
;   ...(Animal-name)...(Animal-species)...(Animal-age)...(Animal-breakfast)...(Animal-dinner)...)

; Exercise 7: Design the function re-assign. It consumes two pieces
;  of data: a PhD student and a grant id. The result is a new PhD
;  whose grant field contains id regardless of what was in there before.

; re-assign : phd string -> phd
; Consumes two pieces of data: a PhD student phd-student and a grant id phd-grant,
;  resulting in a new PhD whose grant field contains id.
; (define (re-assign phd)
;   ... (phd-name) ... (phd-grant)...)

(define (re-assign phd-student phd-grant)
  (make-phd (phd-name phd-student) phd-grant (phd-pay-rate phd-student)))
  
(check-expect (re-assign Susie "12345123")
              (make-phd "Susie" "12345123" 50000))
(check-expect (re-assign Jill "99999999")
              (make-phd "Jill" "99999999" 95203))
