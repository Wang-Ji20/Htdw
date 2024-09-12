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

(define-struct velocity (x y))

(define-struct player (velocity pos cd))

(define-struct enemy (velocity pos))

(define-struct world (player enemy projectiles))

(define start-state
  (make-world
   (make-player (make-velocity 0 0) (make-posn 0 0) 0)
   (cons (make-enemy (make-velocity 20 0) (make-posn 200 200))
         (cons (make-enemy (make-velocity 20 0) (make-posn 100 100)) '()))
   '()
   ))

(define WIDTH 600)
(define HEIGHT 800)

(define empty-velocity (make-velocity 0 0))

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

; accelarate player in the world

(define (accelarate velocity world)
  (let ([player (world-player world)])
    (make-world
     (make-player velocity
                  (player-pos player)
                  (player-cd player))
     (world-enemy world)
     (world-projectiles world))))

(define BACKGROUND
  (empty-scene WIDTH HEIGHT))

; move the player on key event
(define (alter-player-on-key world key)
  (cond
    [(key=? key "w") (accelarate (make-velocity 0 (- 10)) world)]
    [(key=? key "s") (accelarate (make-velocity 0 10) world)]
    [(key=? key "a") (accelarate (make-velocity (- 10) 0) world)]
    [(key=? key "d") (accelarate (make-velocity 10 0) world)]
    [else world]))

;==============================================  Tick  ==============================================

; on each tick, we let
; 1) player emit one projectile, clear cd
; 2) move the projectile and enemy.
; 3) clear projectiles that touch the wall

(define (move s v)
  (make-posn (confine-pos-x (+ (posn-x s) (velocity-x v)))
             (confine-pos-y (+ (posn-y s) (velocity-y v)))))

(define CD 10)

(define (player-tick player)
  (let ([pos (player-pos player)]
        [velocity (player-velocity player)]
        [cd (player-cd player)])
    (make-player
     empty-velocity
     (move pos velocity)
     (cond
       [(eq? cd 0) CD]
       [else (- cd 1)]))))

(define (remove-enemy enemies projectiles)
  (filter (λ (enemy)
            (noneof projectiles
                    (λ (projectile) (equal? (enemy-pos enemy) projectile))))
          enemies))

; TODO: move enemies, left -> right -> left ...
; TODO: fix: the projectiles are by jump, rather than by slide
; TODO: spawn enemies
; TODO: enemy spawn projectiles
(define (move-enemy enemy)
  (let ([pos (enemy-pos enemy)]
        [velocity (enemy-velocity enemy)])
    (make-enemy velocity (move pos velocity))))

(define (enemy-tick enemies projectiles)
  (map move-enemy (remove-enemy enemies projectiles)))

(define always-up-velocity (make-velocity 0 (- 10)))

(define (move-projectile p)
  (move p always-up-velocity))

(define (projectiles-tick player projectiles)
  (let ([projectiles (cond
                       [(eq? (player-cd player) CD) (cons (player-pos player) projectiles)]
                       [else projectiles])])
    (filter (λ (projectile)
              (not (eq? (posn-y projectile) 0)))
            (map move-projectile projectiles))))

(define (world-tick world)
  (let ([player (world-player world)]
        [enemy  (world-enemy world)]
        [projectiles (world-projectiles world)])
    (make-world
     (player-tick player)
     ; control the enemy on tick. always turn right
     (enemy-tick enemy projectiles)
     (projectiles-tick player projectiles))))

;============================================== Render ==============================================

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
        (λ (entity-pos bg)
          (place-sprite-at-pos-in sprite entity-pos bg))))

; render enemy and player in our world
(define (draw-player-on player canvas)
  (place-sprite-at-pos-in player-sprite (player-pos player) canvas))

(define (draw-enemies-on enemies canvas)
  (place-entities-on (map enemy-pos enemies) canvas))

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
         (λ (enemy) (equal? (enemy-pos enemy) (player-pos (world-player world))))))

(define main
  (big-bang start-state
    [on-tick world-tick]
    [on-key alter-player-on-key]
    [to-draw render]
    [stop-when lose]))
