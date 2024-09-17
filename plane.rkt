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
; - points

(define-struct velocity (x y))

(define-struct player (velocity pos cd))
(define PLAYER-SPEED 10)

; enemy hp:
; - 0: hitted
; - 1: alive
(define-struct enemy (velocity pos hp))
(define ENEMY-SPEED 10)
(define ENEMY-STARTING-VELOCITY (make-velocity ENEMY-SPEED 0))

(define-struct projectile (velocity pos emitter))
(define PROJECTILE-SPEED 50)

(define-struct world (player enemy projectiles enemy-spawn-cd points))
(define WIDTH 600)
(define HEIGHT 800)

(define start-state
  (make-world
   (make-player (make-velocity 0 0) (make-posn WIDTH HEIGHT) 0)
   '()
   '()
   0
   0
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
     (world-projectiles world)
     (world-enemy-spawn-cd world)
     (world-points world))))

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

(define (test-enemy-projectile-hit enemy projectile)
  (and (equal? (projectile-emitter projectile) 'player)
       (hit? projectile enemy)))

(define (test-enemy-hit enemy projectiles)
  (make-enemy
   (enemy-velocity enemy)
   (enemy-pos enemy)
   (cond
     [(noneof projectiles (λ (projectile) (test-enemy-projectile-hit enemy projectile))) (enemy-hp enemy)]
     [else (- 1 (enemy-hp enemy))])))

(define (test-enemies-hit enemies projectiles)
  (map (λ (enemy) (test-enemy-hit enemy projectiles)) enemies))

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
         [velocity (bounce (enemy-velocity enemy) pos)]
         [hp (enemy-hp enemy)])
    (make-enemy velocity (move pos velocity) hp)))

(define ENEMY-SPAWN-POINT (make-posn 200 200))
(define ENEMY-SPAWN-CD 50)

(define (zombie? enemy) (equal? 0 (enemy-hp enemy)))

(define (alive? enemy) (not (zombie? enemy)))

; 1. remove hitted enemy
; 2. test hit
; 3. move enemy
; 4. spawn new enemy.
(define (enemy-tick enemies projectiles enemy-spawn-cd)
  (let* (
         [non-zombie-enemies (filter alive? enemies)]
         [hitted-enemies (test-enemies-hit non-zombie-enemies projectiles)]
         [current-enemies (map move-enemy hitted-enemies)])
    (cond
      [(eq? enemy-spawn-cd 0) (cons (make-enemy ENEMY-STARTING-VELOCITY ENEMY-SPAWN-POINT 1) current-enemies)]
      [else current-enemies])))

(define (make-timer-ticker timeout)
  (λ (current-time) (if (eq? current-time 0) timeout (- current-time 1))))

(define enemy-spawn-timer (make-timer-ticker ENEMY-SPAWN-CD))

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

(define (allthat p? l)
  (cond
    [(eq? l '()) 0]
    [else (let* ([l-car (car l)]
                 [l-cdr (cdr l)]
                 [lcarp? (p? l-car)])
            (+ (if lcarp? 1 0) (allthat p? l-cdr)))]))

; TODO: identify points from enemy-hitted
(define (points-tick points enemies)
  (+ points (allthat zombie? enemies)))

(define (world-tick world)
  (let ([player (world-player world)]
        [enemies  (world-enemy world)]
        [projectiles (world-projectiles world)]
        [spawn-cd (world-enemy-spawn-cd world)]
        [points (world-points world)])
    (make-world
     (player-tick player)
     (enemy-tick enemies projectiles spawn-cd)
     (projectiles-tick player projectiles)
     (enemy-spawn-timer spawn-cd)
     (points-tick points enemies))))

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

(define (draw-points-on points canvas)
  (place-image (text points 30 "red")
               300
               50
               canvas))

(define (render world)
  (let ([player (world-player world)]
        [enemy (world-enemy world)]
        [projectiles (world-projectiles world)]
        [points (world-points world)])
    (draw-points-on (number->string points)
                    (draw-player-on player
                                    (draw-enemies-on enemy
                                                     (draw-projectiles-on projectiles BACKGROUND))))))

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
