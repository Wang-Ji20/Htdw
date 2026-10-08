#lang racket/base

(require racket/set
         racket/list
         "config.rkt"
         "geometry.rkt"
         "entities.rkt"
         "combat.rkt"
         "projectiles.rkt")

(provide (struct-out world)
         world-init
         world-step
         world-key-down
         world-key-up
         world-restart
         compute-player-velocity
         step-enemy
         step-enemy-and-shoot)

;; World State Structure
;; player         : player
;; enemies        : (listof enemy)
;; projectiles    : (listof projectile)
;; enemy-spawn-cd : non-negative integer
;; points         : non-negative integer
;; held-keys      : (set/c any/c) - immutable set of held key codes
;; game-over?     : boolean
(struct world (player enemies projectiles enemy-spawn-cd points held-keys game-over?) #:transparent)

;; Boundary Clampers for Player
(define confine-player-x
  (make-clamper (/ PLAYER-WIDTH 2) (- WIDTH (/ PLAYER-WIDTH 2))))

(define confine-player-y
  (make-clamper (/ PLAYER-HEIGHT 2) (- HEIGHT (/ PLAYER-HEIGHT 2))))

;; Initial World State
(define (world-init)
  (world (make-player (posn (/ WIDTH 2) (- HEIGHT 80)) (velocity 0 0) 0)
         '()
         '()
         0
         0
         (set)
         #f))

;; Restart World State
(define (world-restart)
  (world-init))

;; Key matching helpers
(define (key-up? k)    (or (eq? k 'up)    (eq? k #\w) (eq? k #\W)))
(define (key-down? k)  (or (eq? k 'down)  (eq? k #\s) (eq? k #\S)))
(define (key-left? k)  (or (eq? k 'left)  (eq? k #\a) (eq? k #\A)))
(define (key-right? k) (or (eq? k 'right) (eq? k #\d) (eq? k #\D)))

;; Computes player movement velocity vector from the set of currently held keys
(define (compute-player-velocity held-keys)
  (define has-up    (for/or ([k (in-set held-keys)]) (key-up? k)))
  (define has-down  (for/or ([k (in-set held-keys)]) (key-down? k)))
  (define has-left  (for/or ([k (in-set held-keys)]) (key-left? k)))
  (define has-right (for/or ([k (in-set held-keys)]) (key-right? k)))

  (define vx
    (cond
      [(and has-left (not has-right))  (- PLAYER-SPEED)]
      [(and has-right (not has-left))  PLAYER-SPEED]
      [else 0]))

  (define vy
    (cond
      [(and has-up (not has-down))    (- PLAYER-SPEED)]
      [(and has-down (not has-up))    PLAYER-SPEED]
      [else 0]))

  (velocity vx vy))

;; Pure Input: register key press
(define (world-key-down w key)
  (struct-copy world w [held-keys (set-add (world-held-keys w) key)]))

;; Pure Input: register key release
(define (world-key-up w key)
  (struct-copy world w [held-keys (set-remove (world-held-keys w) key)]))

;; Enemy bouncing with velocity-direction checking
(define (bounce-enemy-velocity vel pos)
  (define vx (velocity-x vel))
  (define vy (velocity-y vel))
  (define x (posn-x pos))
  (define y (posn-y pos))
  (define half-w (/ ENEMY-WIDTH 2))
  (define half-h (/ ENEMY-HEIGHT 2))

  (define next-vx
    (cond
      [(and (<= x half-w) (< vx 0))             (- vx)]
      [(and (>= x (- WIDTH half-w)) (> vx 0))    (- vx)]
      [else vx]))

  (define next-vy
    (cond
      [(and (<= y half-h) (< vy 0))             (- vy)]
      [(and (>= y (- HEIGHT half-h)) (> vy 0))   (- vy)]
      [else vy]))

  (velocity next-vx next-vy))

;; Move enemy entity
(define (step-enemy e)
  (define v (bounce-enemy-velocity (enemy-velocity e) (enemy-pos e)))
  (define p (posn+vec (enemy-pos e) v))
  (struct-copy enemy e [velocity v] [pos p]))

;; Steps enemy movement and projectile emission
;; Returns: (values next-enemy-state (listof projectile))
(define (step-enemy-and-shoot e target-p-pos)
  (define v (bounce-enemy-velocity (enemy-velocity e) (enemy-pos e)))
  (define p (posn+vec (enemy-pos e) v))
  (define cd (enemy-shoot-cd e))
  (define pat (enemy-pattern e))
  (if (<= cd 0)
      (let ([fired-projs (emit-enemy-projectiles pat p target-p-pos)]
            [next-pat (advance-enemy-pattern pat)])
        (values (enemy v p (enemy-hp e) ENEMY-SHOOT-CD next-pat)
                fired-projs))
      (values (enemy v p (enemy-hp e) (- cd 1) pat)
              '())))

;; Pure Simulation Step
(define (world-step w)
  (if (world-game-over? w)
      w
      (let* ([p (world-player w)]
             [enemies (world-enemies w)]
             [projectiles (world-projectiles w)]
             [spawn-cd (world-enemy-spawn-cd w)]
             [points (world-points w)]
             [held-keys (world-held-keys w)])

        ;; 1. Update Player Movement & Cooldown
        (define p-vel (compute-player-velocity held-keys))
        (define p-cd (player-cd p))
        (define next-p-pos
          (posn (confine-player-x (+ (posn-x (player-pos p)) (velocity-x p-vel)))
                (confine-player-y (+ (posn-y (player-pos p)) (velocity-y p-vel)))))
        (define firing? (<= p-cd 0))
        (define next-p-cd (if firing? PLAYER-COOLDOWN (- p-cd 1)))
        (define next-player (player p-vel next-p-pos next-p-cd))

        ;; 2. Player Firing Projectiles
        (define player-projs
          (if firing?
              (list (make-player-projectile next-p-pos (velocity 0 (- PROJECTILE-SPEED))))
              '()))

        ;; 3. Enemy Spawning, Movement & Shooting
        (define spawn-ready? (<= spawn-cd 0))
        (define next-spawn-cd (if spawn-ready? ENEMY-SPAWN-CD (- spawn-cd 1)))
        (define enemies-with-spawn
          (if spawn-ready?
              (cons (make-enemy (posn ENEMY-SPAWN-X ENEMY-SPAWN-Y)
                                (velocity ENEMY-SPEED 0)
                                1)
                    enemies)
              enemies))

        (define-values (moved-enemies new-enemy-projs)
          (for/fold ([stepped-enemies '()]
                     [all-emitted '()])
                    ([e (in-list enemies-with-spawn)])
            (define-values (next-e emitted) (step-enemy-and-shoot e next-p-pos))
            (values (cons next-e stepped-enemies)
                    (append emitted all-emitted))))

        ;; 4. Move Projectiles & Prune Off-screen
        (define all-projs (append player-projs new-enemy-projs projectiles))
        (define moved-projs
          (filter projectile-alive? (map step-projectile all-projs)))

        ;; 5. Combat Resolution
        (define-values (surv-projs surv-enemies gained-points)
          (resolve-combat moved-projs (reverse moved-enemies)))

        ;; 6. Check Game Over Condition
        (define hit-by-enemy?
          (for/or ([e (in-list surv-enemies)])
            (player-collides-enemy? next-player e)))
        (define hit-by-bullet?
          (for/or ([b (in-list surv-projs)])
            (player-collides-projectile? next-player b)))
        (define is-game-over? (or hit-by-enemy? hit-by-bullet?))

        (world next-player
               surv-enemies
               surv-projs
               next-spawn-cd
               (+ points gained-points)
               held-keys
               is-game-over?))))

;; ====================================================================
;; Unit Tests
;; ====================================================================

(module+ test
  (require rackunit)

  (define w0 (world-init))
  (check-false (world-game-over? w0))
  (check-equal? (world-points w0) 0)

  ;; Test input velocity derivation
  (check-equal? (compute-player-velocity (set)) (velocity 0 0))
  (check-equal? (compute-player-velocity (set 'up)) (velocity 0 (- PLAYER-SPEED)))
  (check-equal? (compute-player-velocity (set #\w #\d)) (velocity PLAYER-SPEED (- PLAYER-SPEED)))
  (check-equal? (compute-player-velocity (set 'up 'down)) (velocity 0 0))

  ;; Key down / up state changes
  (define w-pressed (world-key-down w0 'left))
  (check-true (set-member? (world-held-keys w-pressed) 'left))
  (define w-released (world-key-up w-pressed 'left))
  (check-false (set-member? (world-held-keys w-released) 'left))

  ;; Simulation step
  (define w1 (world-step w0))
  (check-pred world? w1)

  ;; Enemy spawn on step
  (check-equal? (length (world-enemies w1)) 1 "First step spawns an enemy")

  ;; Enemy shooting triggers when cd is 0
  (define ready-enemy (make-enemy (posn 300 100) (velocity 0 0) 1 0 'vertical))
  (define w-ready (world (make-player (posn 300 700) (velocity 0 0) 10)
                         (list ready-enemy)
                         '()
                         100
                         0
                         (set)
                         #f))
  (define w-after-shot (world-step w-ready))
  (check-true (> (length (world-projectiles w-after-shot)) 0) "Enemy fired projectiles into world")
  (define enemy-bullet (car (world-projectiles w-after-shot)))
  (check-pred enemy-projectile? enemy-bullet)

  ;; Game over condition triggers on enemy collision
  (define p-hit (make-player (posn 200 200) (velocity 0 0) 10))
  (define e-hit (make-enemy (posn 200 200)))
  (define w-doomed (world p-hit (list e-hit) '() 10 0 (set) #f))
  (define w-lost (world-step w-doomed))
  (check-true (world-game-over? w-lost) "Collision with enemy triggers game over")

  ;; Game over condition triggers on enemy bullet hit
  (define p-bullet-target (make-player (posn 300 700) (velocity 0 0) 10))
  (define fatal-bullet (make-enemy-projectile (posn 300 700) (velocity 0 0)))
  (define w-bullet-doom (world p-bullet-target '() (list fatal-bullet) 100 0 (set) #f))
  (define w-bullet-lost (world-step w-bullet-doom))
  (check-true (world-game-over? w-bullet-lost) "Hit by enemy bullet triggers game over"))
