#lang htdp/asl

(require 2htdp/image)
(require 2htdp/universe)

(define (anyof x predicate?)
  (cond
    [(eq? x '()) #f]
    [(predicate? (car x)) #t]
    [else (anyof (cdr x) predicate?)]))

(define (noneof x p?)
  (not (anyof x p?)))

(define sprite (circle 5 "solid" "red"))

(define player-sprite (bitmap "./marisa.png"))

; the *world* contains:
; - the position of player: posn
; - a list of position of enemies: posn list
; - a list of *projectiles*
;   projectiles: TODO: when player or enemy touches other's projectiles, they died.
;                but they will not die if the projectile is emitted by themselves.

(define-struct player (pos cd))

(define-struct world (player enemy projectiles))

(define start-state
  (make-world
   (make-player (make-posn 0 0) 0)
   (cons (make-posn 200 200) (cons (make-posn 100 100) '()))
   '()
   ))

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
(define confine-pos-x (make-clamper 0 WIDTH))

(check-expect (confine-pos-x 10000) WIDTH)
(check-expect (confine-pos-x -11) 0)

; constraint y in the world
(define confine-pos-y (make-clamper 0 HEIGHT))

; move the pos's x coordinate
(define (move-pos-horizontal how-to-move)
  (λ (pos)
    (make-posn (how-to-move (posn-x pos))
               (posn-y pos))))

(define (move-right x)
  (+ x speed))

(define (confined-move-right x)
  (confine-pos-x (move-right x)))

(define move-pos-right
  (move-pos-horizontal confined-move-right))

(define (move-left x) (- x speed))

(define (confined-move-left x) (confine-pos-x (move-left x)))

(define move-pos-left
  (move-pos-horizontal confined-move-left))

; move pos's y coordinate

(define down-speed (/ speed 2))

(define (move-pos-vertical how-to-move)
  (λ (pos)
    (make-posn (posn-x pos)
               (how-to-move (posn-y pos)))))

(define (move-down x)
  (+ x down-speed))

(define (confined-move-down x)
  (confine-pos-y (move-down x)))

(define move-pos-down (move-pos-vertical confined-move-down))

(define (move-up x) (- x speed))

(define (confined-move-up x) (confine-pos-y (move-up x)))

(define move-pos-up (move-pos-vertical confined-move-up))

; move player in the world

(define (move-player how-to-move world)
  (let ([player (world-player world)])
    (make-world
     (make-player (how-to-move (player-pos player))
                  (player-cd player))
     (world-enemy world)
     (world-projectiles world))))

(define BACKGROUND
  (empty-scene WIDTH HEIGHT))

; move the player on key event
(define (alter-player-on-key world key)
  (cond
    [(key=? key "w") (move-player move-pos-up world)]
    [(key=? key "s") (move-player move-pos-down world)]
    [(key=? key "a") (move-player move-pos-left world)]
    [(key=? key "d") (move-player move-pos-right world)]
    [else world]))

; on each tick, we let
; 1) player emit one projectile, clear cd
; 2) move the projectile and enemy.
; 3) clear projectiles that touch the wall
;
(define (projectiles-tick player projectiles)
  (let ([projectiles (cond
                       [(eq? (player-cd player) CD) (cons (player-pos player) projectiles)]
                       [else projectiles])])
    (filter (λ (projectile)
              (not (eq? (posn-y projectile) 0)))
            (map move-pos-up projectiles))))

(define CD 10)

(define (player-tick player)
  (make-player (player-pos player)
               (let ([cd (player-cd player)])
                 (cond
                   [(eq? cd 0) CD]
                   [else (- cd 1)]))))

(define (remove-enemy enemies projectiles)
  (filter (λ (enemy)
            (noneof projectiles
                    (λ (projectile) (equal? enemy projectile))))
          enemies))

(define (enemy-tick enemies projectiles)
  (map move-pos-right (remove-enemy enemies projectiles)))

(define (world-tick world)
  (let ([player (world-player world)]
        [enemy  (world-enemy world)]
        [projectiles (world-projectiles world)])
    (make-world
     (player-tick player)
     ; control the enemy on tick. always turn right
     (enemy-tick enemy projectiles)
     (projectiles-tick player projectiles))))

; render the sprite on our world.
(define (place-sprite-at-pos-in sprite x background)
  (place-image sprite
               (posn-x x)
               (posn-y x)
               background))

(define (fold x initState accumulator)
  (cond
    [(eq? x '()) initState]
    [else (fold (cdr x) (accumulator (car x) initState) accumulator)]))

(define (place-entities-on entities bg)
  (fold entities bg
        (λ (enemy-pos bg)
          (place-sprite-at-pos-in sprite enemy-pos bg))))

; render enemy and player in our world
(define (draw-player-on player canvas)
  (place-sprite-at-pos-in player-sprite (player-pos player) canvas))

(define (draw-enemies-on enemies canvas)
  (place-entities-on enemies canvas))

(define (draw-projectiles-on projectiles canvas)
  (place-entities-on projectiles canvas))

(define (render world)
  (let ([player (world-player world)]
        [enemy (world-enemy world)]
        [projectiles (world-projectiles world)])
    (draw-player-on player
                    (draw-enemies-on enemy
                                     (draw-projectiles-on projectiles BACKGROUND)))))

; if the player touches the enemy, we consider the game ends.
(define (lose world)
  (anyof (world-enemy world)
         (λ (enemy) (equal? enemy (world-player world)))))

(define main
  (big-bang start-state
    [on-tick world-tick]
    [on-key alter-player-on-key]
    [to-draw render]
    [stop-when lose]))
