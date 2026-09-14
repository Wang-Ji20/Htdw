#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(provide (all-defined-out))

;; ====================================================================
;; Data Definitions & Structures
;; ====================================================================

(struct posn (x y) #:transparent)
(struct velocity (x y) #:transparent)

(struct player (velocity pos cd) #:transparent)
;; velocity : velocity
;; pos      : posn
;; cd       : non-negative-integer (firing cooldown counter)

(struct enemy (velocity pos hp) #:transparent)
;; velocity : velocity
;; pos      : posn
;; hp       : integer

(struct projectile (velocity pos emitter) #:transparent)
;; velocity : velocity
;; pos      : posn
;; emitter  : 'player | 'enemy

(struct world (player enemies projectiles enemy-spawn-cd points) #:transparent)
;; player         : player
;; enemies        : (listof enemy)
;; projectiles    : (listof projectile)
;; enemy-spawn-cd : non-negative-integer
;; points         : non-negative-integer

;; ====================================================================
;; Constants & Assets
;; ====================================================================

(define WIDTH 600)
(define HEIGHT 800)
(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define PLAYER-SPRITE (bitmap "./marisa.png"))
(define PLAYER-WIDTH (image-width PLAYER-SPRITE))
(define PLAYER-HEIGHT (image-height PLAYER-SPRITE))
(define PLAYER-RADIUS (/ (min PLAYER-WIDTH PLAYER-HEIGHT) 2))
(define PLAYER-SPEED 10)
(define PLAYER-COOLDOWN 3)

(define ENEMY-SPRITE (bitmap "./cirno.png"))
(define ENEMY-WIDTH (image-width ENEMY-SPRITE))
(define ENEMY-HEIGHT (image-height ENEMY-SPRITE))
(define ENEMY-RADIUS (/ (min ENEMY-WIDTH ENEMY-HEIGHT) 2))
(define ENEMY-SPEED 8)
(define ENEMY-SPAWN-CD 45)
(define ENEMY-SPAWN-POINT (posn 200 120))
(define ENEMY-STARTING-VELOCITY (velocity ENEMY-SPEED 0))

(define PROJECTILE-RADIUS 5)
(define PROJECTILE-SPEED 40)
(define PROJECTILE-SPRITE (circle PROJECTILE-RADIUS "solid" "red"))
(define PROJECTILE-UP-VELOCITY (velocity 0 (- PROJECTILE-SPEED)))

(define EMPTY-VELOCITY (velocity 0 0))

;; ====================================================================
;; Math & Vector Utilities
;; ====================================================================

;; (make-clamper cmin cmax) -> (real? -> real?)
(define ((make-clamper cmin cmax) x)
  (cond
    [(< x cmin) cmin]
    [(> x cmax) cmax]
    [else x]))

(define confine-player-x
  (make-clamper (/ PLAYER-WIDTH 2) (- WIDTH (/ PLAYER-WIDTH 2))))

(define confine-player-y
  (make-clamper (/ PLAYER-HEIGHT 2) (- HEIGHT (/ PLAYER-HEIGHT 2))))

;; Add velocity components
(define (accelerate a v)
  (velocity (+ (velocity-x a) (velocity-x v))
            (+ (velocity-y a) (velocity-y v))))

;; Translate posn by velocity
(define (posn-translate p v)
  (posn (+ (posn-x p) (velocity-x v))
        (+ (posn-y p) (velocity-y v))))

;; Squared Euclidean distance between two posn points
(define (distance-sqr p1 p2)
  (+ (sqr (- (posn-x p1) (posn-x p2)))
     (sqr (- (posn-y p1) (posn-y p2)))))

;; Move player position with boundary confinement
(define (move-player-pos pos vel)
  (posn (confine-player-x (+ (posn-x pos) (velocity-x vel)))
        (confine-player-y (+ (posn-y pos) (velocity-y vel)))))

;; ====================================================================
;; Initial State
;; ====================================================================

(define start-player
  (player EMPTY-VELOCITY
          (posn (/ WIDTH 2) (- HEIGHT 80))
          0))

(define start-state
  (world start-player
         '()
         '()
         0
         0))

;; ====================================================================
;; Input Handling
;; ====================================================================

;; Updates player velocity on key press without overriding independent axes
(define (alter-player-on-key w key)
  (define p (world-player w))
  (define v (player-velocity p))
  (define vx (velocity-x v))
  (define vy (velocity-y v))
  (define new-v
    (cond
      [(or (key=? key "w") (key=? key "up"))    (velocity vx (- PLAYER-SPEED))]
      [(or (key=? key "s") (key=? key "down"))  (velocity vx PLAYER-SPEED)]
      [(or (key=? key "a") (key=? key "left"))  (velocity (- PLAYER-SPEED) vy)]
      [(or (key=? key "d") (key=? key "right")) (velocity PLAYER-SPEED vy)]
      [else v]))
  (struct-copy world w [player (struct-copy player p [velocity new-v])]))

;; Cancels only the velocity axis of the released key, preventing sudden full stops
(define (clear-player-velocity w key)
  (define p (world-player w))
  (define v (player-velocity p))
  (define vx (velocity-x v))
  (define vy (velocity-y v))
  (define new-v
    (cond
      [(and (or (key=? key "w") (key=? key "up")) (< vy 0))    (velocity vx 0)]
      [(and (or (key=? key "s") (key=? key "down")) (> vy 0))  (velocity vx 0)]
      [(and (or (key=? key "a") (key=? key "left")) (< vx 0))  (velocity 0 vy)]
      [(and (or (key=? key "d") (key=? key "right")) (> vx 0)) (velocity 0 vy)]
      [else v]))
  (struct-copy world w [player (struct-copy player p [velocity new-v])]))

;; ====================================================================
;; Physics, Collisions & Tick Functions
;; ====================================================================

;; Bounces an enemy off world boundaries by inspecting position and velocity direction
(define (bounce vel pos)
  (define vx (velocity-x vel))
  (define vy (velocity-y vel))
  (define x (posn-x pos))
  (define y (posn-y pos))
  (define half-w (/ ENEMY-WIDTH 2))
  (define half-h (/ ENEMY-HEIGHT 2))
  (velocity
   (cond
     [(and (<= x half-w) (< vx 0)) (- vx)]
     [(and (>= x (- WIDTH half-w)) (> vx 0)) (- vx)]
     [else vx])
   (cond
     [(and (<= y half-h) (< vy 0)) (- vy)]
     [(and (>= y (- HEIGHT half-h)) (> vy 0)) (- vy)]
     [else vy])))

(define (move-enemy e)
  (define v (bounce (enemy-velocity e) (enemy-pos e)))
  (define p (posn-translate (enemy-pos e) v))
  (struct-copy enemy e [velocity v] [pos p]))

(define (move-projectile proj)
  (struct-copy projectile proj [pos (posn-translate (projectile-pos proj) (projectile-velocity proj))]))

(define (projectile-in-bounds? proj)
  (define p (projectile-pos proj))
  (define x (posn-x p))
  (define y (posn-y p))
  (and (<= (- PROJECTILE-RADIUS) x (+ WIDTH PROJECTILE-RADIUS))
       (<= (- PROJECTILE-RADIUS) y (+ HEIGHT PROJECTILE-RADIUS))))

;; Collision between a single projectile and an enemy
(define (projectile-hits-enemy? proj e)
  (and (eq? (projectile-emitter proj) 'player)
       (< (distance-sqr (projectile-pos proj) (enemy-pos e))
          (sqr (+ ENEMY-RADIUS PROJECTILE-RADIUS)))))

;; Resolves collisions between player projectiles and enemies.
;; Consumes projectiles that hit, destroys hit enemies, and returns points earned.
(define (resolve-combat projectiles enemies)
  (define-values (remaining-projs remaining-enemies points-earned)
    (for/fold ([surviving-projs '()]
               [surviving-enemies enemies]
               [points 0])
              ([proj (in-list projectiles)])
      (if (eq? (projectile-emitter proj) 'player)
          (let loop ([checked '()] [to-check surviving-enemies])
            (cond
              [(empty? to-check)
               (values (cons proj surviving-projs) checked points)]
              [(projectile-hits-enemy? proj (first to-check))
               (values surviving-projs
                       (append checked (rest to-check))
                       (+ points 1))]
              [else
               (loop (cons (first to-check) checked) (rest to-check))]))
          (values (cons proj surviving-projs) surviving-enemies points))))
  (values (reverse remaining-projs) remaining-enemies points-earned))

;; Timer ticker helper
(define ((make-timer-ticker timeout) current-time)
  (if (<= current-time 0) timeout (- current-time 1)))

(define enemy-spawn-timer (make-timer-ticker ENEMY-SPAWN-CD))

;; World tick: updates player, spawns/moves enemies and projectiles, resolves collisions
(define (world-tick w)
  (define p (world-player w))
  (define old-enemies (world-enemies w))
  (define old-projectiles (world-projectiles w))
  (define spawn-cd (world-enemy-spawn-cd w))
  (define points (world-points w))

  ;; 1. Update player position and firing cooldown
  (define p-cd (player-cd p))
  (define next-p-pos (move-player-pos (player-pos p) (player-velocity p)))
  (define next-p-cd (if (<= p-cd 0) PLAYER-COOLDOWN (- p-cd 1)))
  (define next-player (struct-copy player p [pos next-p-pos] [cd next-p-cd]))

  ;; 2. Player auto-firing projectile when cooldown hits 0
  (define fired-projectiles
    (if (<= p-cd 0)
        (cons (projectile (accelerate (player-velocity p) PROJECTILE-UP-VELOCITY)
                          (player-pos p)
                          'player)
              old-projectiles)
        old-projectiles))

  ;; 3. Move projectiles and prune out-of-bounds
  (define moved-projectiles
    (filter projectile-in-bounds? (map move-projectile fired-projectiles)))

  ;; 4. Spawn & move enemies
  (define enemies-after-spawn
    (if (<= spawn-cd 0)
        (cons (enemy ENEMY-STARTING-VELOCITY ENEMY-SPAWN-POINT 1) old-enemies)
        old-enemies))
  (define moved-enemies (map move-enemy enemies-after-spawn))

  ;; 5. Resolve projectile vs enemy collisions
  (define-values (surviving-projs surviving-enemies gained-points)
    (resolve-combat moved-projectiles moved-enemies))

  (world next-player
         surviving-enemies
         surviving-projs
         (enemy-spawn-timer spawn-cd)
         (+ points gained-points)))

;; ====================================================================
;; Game Over & Loss Condition
;; ====================================================================

(define (player-hit? p enemies projectiles)
  (define p-pos (player-pos p))
  (or (ormap (λ (e)
               (< (distance-sqr (enemy-pos e) p-pos)
                  (sqr (+ PLAYER-RADIUS ENEMY-RADIUS))))
             enemies)
      (ormap (λ (proj)
               (and (eq? (projectile-emitter proj) 'enemy)
                    (< (distance-sqr (projectile-pos proj) p-pos)
                       (sqr (+ PLAYER-RADIUS PROJECTILE-RADIUS)))))
             projectiles)))

(define (lose? w)
  (player-hit? (world-player w)
               (world-enemies w)
               (world-projectiles w)))

;; ====================================================================
;; Rendering
;; ====================================================================

(define (place-sprite sprite pos scene)
  (place-image sprite (posn-x pos) (posn-y pos) scene))

(define (draw-entities entities get-pos sprite scene)
  (foldl (λ (entity acc)
           (place-sprite sprite (get-pos entity) acc))
         scene
         entities))

(define (render w)
  (define with-projectiles
    (draw-entities (world-projectiles w) projectile-pos PROJECTILE-SPRITE BACKGROUND))
  (define with-enemies
    (draw-entities (world-enemies w) enemy-pos ENEMY-SPRITE with-projectiles))
  (define with-player
    (place-sprite PLAYER-SPRITE (player-pos (world-player w)) with-enemies))
  (place-image (text (format "SCORE: ~a" (world-points w)) 22 "yellow")
               (/ WIDTH 2)
               30
               with-player))

(define (render-game-over w)
  (define base (render w))
  (overlay (above (text "GAME OVER" 46 "red")
                  (text (format "Final Score: ~a" (world-points w)) 24 "white"))
           base))

;; ====================================================================
;; Main Runner
;; ====================================================================

(define (run [initial-state start-state])
  (big-bang initial-state
    [on-tick world-tick]
    [on-key alter-player-on-key]
    [on-release clear-player-velocity]
    [to-draw render]
    [stop-when lose? render-game-over]))

(module+ main
  (run))

;; ====================================================================
;; Unit Tests
;; ====================================================================

(module+ test
  (require rackunit)

  ;; Clamping
  (check-equal? (confine-player-x -50) (/ PLAYER-WIDTH 2))
  (check-equal? (confine-player-x 10000) (- WIDTH (/ PLAYER-WIDTH 2)))
  (check-equal? (confine-player-y -50) (/ PLAYER-HEIGHT 2))
  (check-equal? (confine-player-y 10000) (- HEIGHT (/ PLAYER-HEIGHT 2)))

  ;; Distance squared
  (check-equal? (distance-sqr (posn 0 0) (posn 3 4)) 25)

  ;; Vector bounce
  (define left-edge (/ ENEMY-WIDTH 2))
  (define right-edge (- WIDTH (/ ENEMY-WIDTH 2)))
  (check-equal? (bounce (velocity -10 0) (posn left-edge 100)) (velocity 10 0))
  (check-equal? (bounce (velocity 10 0) (posn right-edge 100)) (velocity -10 0))
  ;; Doesn't reverse if moving away from edge
  (check-equal? (bounce (velocity 10 0) (posn left-edge 100)) (velocity 10 0))

  ;; Input handling: multi-directional WASD and arrow keys
  (define p0 start-player)
  (define w0 (world p0 '() '() 10 0))
  (define w-up (alter-player-on-key w0 "w"))
  (check-equal? (velocity-y (player-velocity (world-player w-up))) (- PLAYER-SPEED))
  ;; Pressing right adds x velocity without canceling y
  (define w-diag (alter-player-on-key w-up "d"))
  (check-equal? (player-velocity (world-player w-diag)) (velocity PLAYER-SPEED (- PLAYER-SPEED)))
  ;; Releasing 'w' clears only y velocity
  (define w-rel-up (clear-player-velocity w-diag "w"))
  (check-equal? (player-velocity (world-player w-rel-up)) (velocity PLAYER-SPEED 0))
  ;; Releasing 'd' clears x velocity
  (define w-rel-all (clear-player-velocity w-rel-up "d"))
  (check-equal? (player-velocity (world-player w-rel-all)) (velocity 0 0))

  ;; Projectile movement and out-of-bounds pruning
  (define p-live (projectile (velocity 0 -10) (posn 100 100) 'player))
  (define p-dead (projectile (velocity 0 -10) (posn 100 -20) 'player))
  (check-true (projectile-in-bounds? p-live))
  (check-false (projectile-in-bounds? p-dead))

  ;; Collision resolution
  (define hit-proj (projectile (velocity 0 -50) (posn 200 200) 'player))
  (define hit-enemy (enemy (velocity 10 0) (posn 200 200) 1))
  (define-values (surv-p surv-e pts)
    (resolve-combat (list hit-proj) (list hit-enemy)))
  (check-equal? surv-p '() "Hit projectile should be consumed")
  (check-equal? surv-e '() "Hit enemy should be destroyed")
  (check-equal? pts 1 "Points earned should be 1")

  ;; Player loss condition
  (define close-enemy (enemy (velocity 0 0) (posn 300 720) 1))
  (define far-enemy (enemy (velocity 0 0) (posn 100 100) 1))
  (check-true (player-hit? p0 (list close-enemy) '()) "Player should be hit when close to enemy")
  (check-false (player-hit? p0 (list far-enemy) '()) "Player should not be hit when far from enemy")

  ;; Full tick step updates state correctly
  (define next-w (world-tick w0))
  (check-pred world? next-w)
  (check-equal? (world-points next-w) 0))
