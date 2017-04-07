;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname assign4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CMPU-101 
; Fall 2016
; Assign 4
; Norman Kuang 
;
; Description: For this assignment, we will design a world
;  (big-bang) program that implements a simple version of
;  classic Tetris game, as described in Section 13.6 of
;  How to Design Programs, Second Edition.

(require 2htdp/image) 
(require 2htdp/universe)


(define WIDTH 10) ; # of blocks, horizontally
(define SIZE 40) ; blocks are squares
(define SCENE-SIZE (* WIDTH SIZE))

(define BLOCK ; red squares with black rims
  (overlay
   (square (- SIZE 1) "solid" "red")
   (square SIZE "outline" "black")))

(define-struct tetris (block landscape))
(define-struct block (x y))

; A Tetris is a structure:
;  (make-tetris block list-of-blocks)
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting

; A Landscape is one of:
;  - '()
;  - (cons Block Landscape)

; A Block is a structure:
;  (make-block num num)


;  We will choose logical representation to represent the
;  the position of the block.

; physical-x : num -> num
; Computes a physical x-coordinate from a given logical
;  coordinate x.
(define (physical-x x)
  (+ (* SIZE x) (* SIZE 0.5)))

(check-expect (physical-x 0) 20)
(check-expect (physical-x 1) 60)

; physical-y : num -> num
; Computes a physical y-coordinate from given logical
;  coordinate y.
(define (physical-y y)
  (+ (* (image-height BLOCK) y) (* (image-height BLOCK) 0.5)))

(check-expect (physical-y 0) 20)
(check-expect (physical-y 1) 60)

(define BACKGROUND (empty-scene SCENE-SIZE SCENE-SIZE))

; Test Examples
(define testblock1 (make-block (physical-x 2) (physical-y 2)))
(define testblock2 (make-block (physical-x 0) (physical-y 9)))
(define tetriscollide (make-tetris (make-block (physical-x 0) (physical-y 8))
                            (list (make-block (physical-x 1) (physical-y 9))
                                  (make-block (physical-x 0) (physical-y 9)))))
(define tetrisfloor (make-tetris (make-block (physical-x 0) (physical-y 9))
                                 (list '())))



; dropblock : tetris -> tetris
; Changes the y-coordinate of the given block in tetris t by a
;  fixed amount.
(define (dropblock t)
  (make-tetris (make-block (block-x (tetris-block t)) (+ (block-y (tetris-block t)) 40))
               (tetris-landscape t)))

(check-expect (dropblock (make-tetris (make-block (physical-x 0) (physical-y 0)) '()))
                         (make-tetris (make-block (physical-x 0) (physical-y 1)) '()))
(check-expect (dropblock (make-tetris (make-block (physical-x 2) (physical-y 2)) '()))
                         (make-tetris (make-block (physical-x 2) (physical-y 3)) '()))

; touchfloor? : tetris -> bool
; Determines if the block has hit the floor of the grid.
(define (touchfloor? t)
  (= (block-y (tetris-block t)) (- SCENE-SIZE (* 0.5 SIZE))))

(check-expect (touchfloor? tetrisfloor) true)
(check-expect (collide? (make-tetris (make-block (physical-x 0) (physical-y 0)) '()))
              false)



; touchblock? : tetris -> bool
; Determines if one block has collided with another block.
(define (touchblock? t)
  (member? (tetris-block (dropblock t)) (tetris-landscape t)))

(check-expect (touchblock? tetriscollide) true)
(check-expect (touchblock? (make-tetris (make-block (physical-x 0) (physical-y 0))
                                        (list '()))) false)

; generateblock : tetris -> tetris
; Generates a block
(define (generateblock t)
 (local [(define n (random WIDTH))]
   (make-tetris (make-block (physical-x n) (physical-y -1))
                (cons (tetris-block t) (tetris-landscape t)))))

(check-random (generateblock tetriscollide)
              (make-tetris (make-block (physical-x (random WIDTH)) (physical-y -1))
                           (list (make-block (physical-x 0) (physical-y 8))
                                  (make-block 60 380) (make-block 20 380))))

; collide? : tetris -> tetris
; Determines if a block has collided with another block or hit the floor.
(define (collide? t)
  (or (touchfloor? t) (touchblock? t)))

(check-expect (collide? tetriscollide) true)
(check-expect (collide? (make-tetris (make-block (physical-x 0) (physical-y 0)) '()))
              false)


; downcollide : tetris -> tetris
; Generates a block and moves it down by a set amount if tetris-block t has
;  collided
(define (downcollide t)
  (if (collide? t) (dropblock (generateblock t)) (dropblock t)))

(check-random (downcollide tetriscollide)
              (make-tetris (make-block (physical-x (random WIDTH)) (physical-y 0))
                           (list (make-block (physical-x 0) (physical-y 8))
                                  (make-block 60 380) (make-block 20 380))))

; moveblockright : tetris -> tetris
; Moves a block one column to the right
(define (moveblockright t)
  (cond
    [(= (block-x (tetris-block t)) (- SCENE-SIZE 20)) t]
    [else (make-tetris (make-block (+ (block-x (tetris-block t)) SIZE) (block-y (tetris-block t)))
                           (tetris-landscape t))]))

 

(check-expect (moveblockright
               (make-tetris (make-block (physical-x 1) (physical-y 1)) '()))
              (make-tetris (make-block (physical-x 2) (physical-y 1)) '()))

; moveblockleft : tetris -> tetris
; Moves a block one column to the left
(define (moveblockleft t)
   (cond
    [(= (block-x (tetris-block t)) 20) t]
    [else (make-tetris (make-block (- (block-x (tetris-block t)) SIZE) (block-y (tetris-block t)))
                           (tetris-landscape t))]))

(check-expect (moveblockleft
               (make-tetris (make-block (physical-x 1) (physical-y 1)) '()))
              (make-tetris (make-block (physical-x 0) (physical-y 1)) '()))


              

; BIG BANG FUNCTIONS


; render-landscape : landscape -> image
; Consumes a landscape and produces an image
(define (render-landscape ls)
  (cond
    [(empty? ls) BACKGROUND]
    [else (place-image BLOCK (block-x (first ls))
                       (block-y (first ls))
                       (render-landscape (rest ls)))]))
 

; render-tetris : tetris -> image
; Consumes a tetris and produces an image
(define (render-tetris t)
  (place-image BLOCK (block-x (tetris-block t)) (block-y (tetris-block t))
               (render-landscape (tetris-landscape t))))

; tetris-left-right : tetris keyevent -> tetris
; Moves the block left or right
(define (tetris-left-right t a-key)
  (cond
    [(key=? a-key "left") (moveblockleft t)]
    [(key=? a-key "right") (moveblockright t)]
    [else t]))

; gameover? tetris -> bool
; Determines if the game is over
(define (gameover? t)
  (> (length (filter (lambda (n) (= (block-y n) (physical-x 0))) (tetris-landscape t))) 0))

; last-picture : tetris -> img
; Consumes a tetris t, and places an image that determines when the game is lost.
(define (last-picture t)
  (place-image (text "Game Over!" 30 "black") 200 180
               (place-image (text "Score:" 30 "black") 200 220
                            (place-image (text (number->string (length (tetris-landscape t))) 30 "black") 200 260
                                               (render-tetris t)))))
                          
; MAIN FUNCTION
(define (tetris-main _)
  (big-bang (make-tetris (make-block (physical-x 4) (physical-y 0)) '())
            [to-draw render-tetris]
            [on-tick downcollide]
            [on-key tetris-left-right]
            [stop-when gameover? last-picture]))

(tetris-main 1)










