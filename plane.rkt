;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname plane) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define sprite (circle 5 "solid" "red"))

; the *world* contains:
; - the position of player: posn
; - a list of position of enemies: posn list

(define-struct world (player enemy))

(define start-state
  (make-world
   (make-posn 0 0)
   (cons (make-posn 200 200) (cons (make-posn 100 100) '()))))

(define WIDTH 600)
(define HEIGHT 800)

(define speed 10)

; clamp the data between..
; lower-bound, higher-bound -> x -> x'
; where x is between lower-bound and higher-bound
; requires cmin < cmax
(define (make-clamper cmin cmax)
  (λ (x)
    (cond
      [(< x cmin) cmin]
      [(> x cmax) cmax]
      [else x])))

; constraint x in the world.
(define confine-world-x (make-clamper 0 WIDTH))

(check-expect (confine-world-x 10000) WIDTH)
(check-expect (confine-world-x -11) 0)

; constraint y in the world
(define confine-world-y (make-clamper 0 HEIGHT))

; move the sprite's x coordinate
(define (move-sprite-horizontal how-to-move)
  (λ (sprite)
    (make-posn (how-to-move (posn-x sprite))
               (posn-y sprite))))

(define (move-right x)
  (+ x speed))

(define (confined-move-right x)
  (confine-world-x (move-right x)))

(define move-sprite-right
  (move-sprite-horizontal confined-move-right))

(define (move-left x) (- x speed))

(define (confined-move-left x) (confine-world-x (move-left x)))

(define move-sprite-left
  (move-sprite-horizontal confined-move-left))
  
; move the sprite's y coordinate

(define down-speed (/ speed 2))

(define (move-sprite-vertical how-to-move)
  (λ (sprite)
    (make-posn (posn-x sprite)
               (how-to-move (posn-y sprite)))))

(define (move-down x)
  (+ x down-speed))

(define (confined-move-down x)
  (confine-world-y (move-down x)))

(define move-sprite-down (move-sprite-vertical confined-move-down))

(define (move-up x) (- x speed))

(define (confined-move-up x) (confine-world-y (move-up x)))

(define move-sprite-up (move-sprite-vertical confined-move-up))

(define (move-sprite sprite)
  (move-sprite-right (move-sprite-down sprite)))

; move player in the world

(define (move-player how-to-move world)
  (make-world
   (how-to-move (world-player world))
   (world-enemy world)))

(define (move-enemies how-to-move world)
  (make-world
   (world-player world)
   (map how-to-move (world-enemy world))))

(check-expect
 (move-sprite (make-posn 300 300))
 (make-posn 310 305))

(define BACKGROUND
  (empty-scene WIDTH HEIGHT))

; move the player on key event
(define (alter-player-on-key world key)
  (cond
    [(key=? key "w") (move-player move-sprite-up world)]
    [(key=? key "s") (move-player move-sprite-down world)]
    [(key=? key "a") (move-player move-sprite-left world)]
    [(key=? key "d") (move-player move-sprite-right world)]
    [else world]))

; control the enemy on tick. always turn right
(define (move-enemy-in-world world)
  (move-enemies move-sprite-right world))

; render the sprite on our world.
(define (place-sprite-at-pos x background)
  (place-image sprite
               (posn-x x)
               (posn-y x)
               background))

(define (fold x initState accumulator)
  (cond
    [(eq? x '()) initState]
    [else (fold (cdr x) (accumulator (car x) initState) accumulator)]))

(define (place-enemies world)
  (fold (world-enemy world) BACKGROUND
        (λ (enemy-pos bg)
          (place-sprite-at-pos enemy-pos bg))))

; render enemy and player in our world
(define (render world)
  (place-sprite-at-pos (world-player world)
                       (place-enemies world)))

(define main
  (big-bang start-state
    [on-tick move-enemy-in-world]
    [on-key alter-player-on-key]
    [to-draw render]))
