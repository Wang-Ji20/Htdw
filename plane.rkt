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

(define projectile-sprite (circle 5 "solid" "red"))

(define player-sprite (bitmap "./marisa.png"))

(define enemy-sprite (bitmap "./cirno.png"))

; the *world* contains:
; - the position of player: posn
; - a list of position of enemies: posn list
; - a list of *projectiles*
;   projectiles: TODO: when player or enemy touches other's projectiles, they died.
;                but they will not die if the projectile is emitted by themselves.

(define-struct velocity (x y))

(define-struct player (velocity pos cd))
(define PLAYER-SPEED 10)

(define-struct enemy (velocity pos))
(define ENEMY-SPEED 10)

(define-struct projectile (velocity pos emitter))
(define PROJECTILE-SPEED 50)

(define-struct world (player enemy projectiles))
(define WIDTH 600)
(define HEIGHT 800)

(define start-state
  (make-world
   (make-player (make-velocity 0 0) (make-posn WIDTH HEIGHT) 0)
   (cons (make-enemy (make-velocity ENEMY-SPEED 0) (make-posn 200 200))
         (cons (make-enemy (make-velocity ENEMY-SPEED 0) (make-posn 100 100)) '()))
   '()
   ))

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

; accelerate
(define (accelerate a v)
  (make-velocity (+ (velocity-x a) (velocity-x v))
                 (+ (velocity-y a) (velocity-y v))))

; set player velocity in the world

(define (set-velocity velocity world)
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
    [(key=? key "w") (set-velocity (make-velocity 0 (- PLAYER-SPEED)) world)]
    [(key=? key "s") (set-velocity (make-velocity 0 PLAYER-SPEED) world)]
    [(key=? key "a") (set-velocity (make-velocity (- PLAYER-SPEED) 0) world)]
    [(key=? key "d") (set-velocity (make-velocity PLAYER-SPEED 0) world)]
    [else world]))

(define (clear-player-velocity world key)
  (set-velocity empty-velocity world))

;==============================================  Tick  ==============================================

; on each tick, we let
; 1) player emit one projectile, clear cd
; 2) move the projectile and enemy.
; 3) clear projectiles that touch the wall

(define (move s v)
  (make-posn (confine-pos-x (+ (posn-x s) (velocity-x v)))
             (confine-pos-y (+ (posn-y s) (velocity-y v)))))

(define CD 2)

(define (player-tick player)
  (let ([pos (player-pos player)]
        [velocity (player-velocity player)]
        [cd (player-cd player)])
    (make-player
     velocity
     (move pos velocity)
     (cond
       [(eq? cd 0) CD]
       [else (- cd 1)]))))

; need projectile radius to calculate the collision between enemy and projectile. If their distance
; is less than 5, then we consider they are collided
(define PROJECTILE-RADIUS 5)

(define (eulicid-distance-square p1 p2)
  (let* ([absx (abs (- (posn-x p1) (posn-x p2)))]
         [absy (abs (- (posn-y p1) (posn-y p2)))])
    (+ (* absx absx) (* absy absy))))

(define ENEMY-HITBOX-DIAMETER 26)

(define (math-square x) (* x x))

(define (hit? projectile enemy)
  (let* ([p-pos (projectile-pos projectile)]
         [e-pos (enemy-pos enemy)])
    (< (eulicid-distance-square p-pos e-pos) (math-square (+ ENEMY-HITBOX-DIAMETER PROJECTILE-RADIUS)))))

(define (remove-enemy enemies projectiles)
  (filter (λ (enemy)
            (noneof projectiles
                    (λ (projectile) (and (equal? (projectile-emitter projectile) 'player)
                                         (hit? projectile enemy)))))
          enemies))

; TODO: spawn enemies
; TODO: enemy spawn projectiles

; if the enemy hits the wall, it bounce by reversing its speed
; pos * velocity -> velocity
(define (bounce velocity position)
  (let ([vx (velocity-x velocity)]
        [vy (velocity-y velocity)]
        [x (posn-x position)]
        [y (posn-y position)])
    (make-velocity (if (or (eq? x WIDTH) (eq? x 0)) (- vx) vx)
                   (if (or (eq? y HEIGHT) (eq? y 0)) (- vy) vy))))

(check-expect (bounce (make-velocity 20 0) (make-posn WIDTH 0)) (make-velocity (- 20) 0))

(define (move-enemy enemy)
  (let* ([pos (enemy-pos enemy)]
         [velocity (bounce (enemy-velocity enemy) pos)])
    (make-enemy velocity (move pos velocity))))

(define (enemy-tick enemies projectiles)
  (map move-enemy (remove-enemy enemies projectiles)))

(define always-up-velocity (make-velocity 0 (- PROJECTILE-SPEED)))

(define (move-projectile p)
  (let ([vel (projectile-velocity p)])
    (make-projectile  vel
                      (move (projectile-pos p) vel)
                      (projectile-emitter p))))

(define (projectiles-tick player projectiles)
  (let ([projectiles (cond
                       [(eq? (player-cd player) CD) (cons (make-projectile (accelerate (player-velocity player) always-up-velocity)
                                                                           (player-pos player)
                                                                           'player)
                                                          projectiles)]
                       [else projectiles])])
    (filter (λ (projectile)
              (not (eq? (posn-y (projectile-pos projectile)) 0)))
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

(define (place-entities-on entities sprite bg)
  (fold entities bg
        (λ (entity-pos bg)
          (place-sprite-at-pos-in sprite entity-pos bg))))

; render enemy and player in our world
(define (draw-player-on player canvas)
  (place-sprite-at-pos-in player-sprite (player-pos player) canvas))

(define (draw-enemies-on enemies canvas)
  (place-entities-on (map enemy-pos enemies) enemy-sprite canvas))

(define (draw-projectiles-on projectiles canvas)
  (place-entities-on (map projectile-pos projectiles) projectile-sprite canvas))

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
    [on-release clear-player-velocity]
    [to-draw render]
    [stop-when lose]))
