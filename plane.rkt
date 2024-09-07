;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname plane) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define sprite (circle 5 "solid" "red"))

; the *world* is a position in the scene

(define world (make-posn 0 0))
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
  
(check-expect
 (move-sprite (make-posn 300 300))
 (make-posn 310 305))

(define BACKGROUND
  (empty-scene WIDTH HEIGHT))

; move the sprite on key event
(define (alter-sprite-on-key sprite key)
  (cond
    [(key=? key "w") (move-sprite-up sprite)]
    [(key=? key "s") (move-sprite-down sprite)]
    [(key=? key "a") (move-sprite-left sprite)]
    [(key=? key "d") (move-sprite-right sprite)]
    [else sprite]))

; render the sprite on our world.
(define (place-sprite-at-pos x)
  (place-image sprite
               (posn-x x)
               (posn-y x)
               BACKGROUND))

(define main
  (big-bang world
    ;[on-tick move-sprite]
    [on-key alter-sprite-on-key]
    [to-draw place-sprite-at-pos]))
