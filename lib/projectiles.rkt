#lang racket/base

(require racket/math
         racket/list
         "config.rkt"
         "geometry.rkt")

(provide (struct-out projectile)
         ;; Predicates
         player-projectile?
         enemy-projectile?
         bouncing-projectile?
         ;; Constructors
         make-projectile
         make-player-projectile
         make-enemy-projectile
         make-vertical-projectile
         make-radial-projectile
         make-aimed-projectile
         make-bouncing-projectile
         ;; Steppers & lifecycle
         step-projectile
         step-linear-projectile
         step-bouncing-projectile
         projectile-alive?
         ;; Pattern emission
         emit-vertical-bullets
         emit-radial-bullets
         emit-aimed-bullets
         emit-bouncing-bullets
         emit-enemy-projectiles
         advance-enemy-pattern
         DEFAULT-ENEMY-PATTERN-CYCLE
         ;; Extensibility registries
         register-projectile-stepper!
         register-pattern-emitter!)

;; ====================================================================
;; 1. Projectile Entity Definition
;; ====================================================================

;; Projectile structure
;; velocity : velocity (vx, vy displacement per frame)
;; pos      : posn     (x, y spatial coordinates)
;; emitter  : 'player | 'enemy
;; type     : symbol (e.g. 'player, 'vertical, 'radial, 'aimed, 'bouncing, or custom)
;; bounces  : integer (remaining boundary bounces allowed)
;; extra    : any/c (optional metadata, payload, or custom stepper procedure)
(struct projectile (velocity pos emitter type bounces extra) #:transparent)

;; Predicates
(define (player-projectile? proj)
  (eq? (projectile-emitter proj) 'player))

(define (enemy-projectile? proj)
  (eq? (projectile-emitter proj) 'enemy))

(define (bouncing-projectile? proj)
  (eq? (projectile-type proj) 'bouncing))

;; Generic Constructor
(define (make-projectile v p emitter [type 'linear] [bounces 0] [extra #f])
  (projectile v p emitter type bounces extra))

;; Player Projectile: straight upward shot
(define (make-player-projectile p v)
  (projectile v p 'player 'player 0 #f))

;; Generic Enemy Projectile
(define (make-enemy-projectile p v [type 'vertical] [bounces 0] [extra #f])
  (projectile v p 'enemy type bounces extra))

;; (1) Vertically Falling Projectile
(define (make-vertical-projectile p [v (velocity 0 ENEMY-BULLET-SPEED)])
  (projectile v p 'enemy 'vertical 0 #f))

;; (2) Radial Projectile
(define (make-radial-projectile p v)
  (projectile v p 'enemy 'radial 0 #f))

;; (3) Aiming at Player Projectile (fixed linear velocity upon emission)
(define (make-aimed-projectile p v)
  (projectile v p 'enemy 'aimed 0 #f))

;; (4) Bouncing Projectile (bounces on screen boundaries)
(define (make-bouncing-projectile p v [bounces BOUNCING-BULLET-BOUNCES])
  (projectile v p 'enemy 'bouncing bounces #f))

;; ====================================================================
;; 2. Projectile Stepping & Physics
;; ====================================================================

;; Linear step: straightforward position update
(define (step-linear-projectile proj)
  (struct-copy projectile proj
               [pos (posn+vec (projectile-pos proj) (projectile-velocity proj))]))

;; Bouncing step: updates position and reflects off screen borders if bounces > 0
(define (step-bouncing-projectile proj)
  (define v (projectile-velocity proj))
  (define p (projectile-pos proj))
  (define b (projectile-bounces proj))

  ;; If no bounces remain, bullet behaves like a linear projectile
  (if (<= b 0)
      (step-linear-projectile proj)
      (let* ([vx (velocity-x v)]
             [vy (velocity-y v)]
             [next-x (+ (posn-x p) vx)]
             [next-y (+ (posn-y p) vy)]
             [r PROJECTILE-RADIUS]
             [min-x r]
             [max-x (- WIDTH r)]
             [min-y r]
             [max-y (- HEIGHT r)]
             ;; Horizontal boundary bounce
             [hit-x? (or (and (<= next-x min-x) (< vx 0))
                         (and (>= next-x max-x) (> vx 0)))]
             [next-vx (if hit-x? (- vx) vx)]
             [clamped-x (cond [(< next-x min-x) min-x]
                              [(> next-x max-x) max-x]
                              [else next-x])]
             ;; Vertical boundary bounce
             [hit-y? (or (and (<= next-y min-y) (< vy 0))
                         (and (>= next-y max-y) (> vy 0)))]
             [next-vy (if hit-y? (- vy) vy)]
             [clamped-y (cond [(< next-y min-y) min-y]
                              [(> next-y max-y) max-y]
                              [else next-y])]
             ;; Decrement bounce count if any wall was struck
             [bounced? (or hit-x? hit-y?)]
             [next-b (if bounced? (- b 1) b)])
        (struct-copy projectile proj
                     [velocity (velocity next-vx next-vy)]
                     [pos (posn clamped-x clamped-y)]
                     [bounces next-b]))))

;; Custom stepper registry for modular extensibility
(define custom-steppers (make-hash))

(define (register-projectile-stepper! type-symbol stepper-proc)
  (hash-set! custom-steppers type-symbol stepper-proc))

;; Stepper dispatcher
(define (step-projectile proj)
  (cond
    ;; 1. Check registered custom steppers
    [(hash-ref custom-steppers (projectile-type proj) #f)
     => (λ (stepper) (stepper proj))]
    ;; 2. Check if extra payload itself is an updater procedure
    [(procedure? (projectile-extra proj))
     ((projectile-extra proj) proj)]
    ;; 3. Built-in bouncing projectile physics
    [(eq? (projectile-type proj) 'bouncing)
     (step-bouncing-projectile proj)]
    ;; 4. Default: linear motion
    [else
     (step-linear-projectile proj)]))

;; Projectile lifetime check (culled when completely out of screen bounds)
(define (projectile-alive? proj)
  (in-bounds? (projectile-pos proj)
              (- PROJECTILE-RADIUS)
              (- PROJECTILE-RADIUS)
              (+ WIDTH PROJECTILE-RADIUS)
              (+ HEIGHT PROJECTILE-RADIUS)))

;; ====================================================================
;; 3. Enemy Projectile Emitter Patterns
;; ====================================================================

;; (1) Vertically Falling Bullets: straight downward drop
(define (emit-vertical-bullets enemy-pos [speed ENEMY-BULLET-SPEED])
  (list (make-vertical-projectile enemy-pos (velocity 0 speed))))

;; (2) Radical / Radial Bullets: omnidirectional ring burst
(define (emit-radial-bullets enemy-pos
                             [count RADIAL-BULLET-COUNT]
                             [speed ENEMY-BULLET-SPEED]
                             [start-angle 0])
  (for/list ([i (in-range count)])
    (define angle (+ start-angle (* i (/ (* 2 pi) count))))
    (define vx (* speed (cos angle)))
    (define vy (* speed (sin angle)))
    (make-radial-projectile enemy-pos (velocity vx vy))))

;; (3) Aiming at Player Bullets (not following):
;; Computes trajectory towards player at launch, then travels linearly without tracking
(define (emit-aimed-bullets enemy-pos player-pos [speed ENEMY-BULLET-SPEED])
  (define dx (- (posn-x player-pos) (posn-x enemy-pos)))
  (define dy (- (posn-y player-pos) (posn-y enemy-pos)))
  (define dist (sqrt (+ (sqr dx) (sqr dy))))
  (define-values (vx vy)
    (if (< dist 0.001)
        (values 0 speed)
        (values (* speed (/ dx dist))
                (* speed (/ dy dist)))))
  (list (make-aimed-projectile enemy-pos (velocity vx vy))))

;; (4) Bouncing Bullet:
;; Emits a bullet capable of reflecting once off screen walls.
;; If target-or-angle is #f, heuristically aims towards the nearer side wall.
(define (emit-bouncing-bullets enemy-pos
                              [target-or-angle #f]
                              [speed ENEMY-BULLET-SPEED]
                              [bounces BOUNCING-BULLET-BOUNCES])
  (define-values (vx vy)
    (cond
      ;; Explicit angle provided (radians)
      [(number? target-or-angle)
       (values (* speed (cos target-or-angle))
               (* speed (sin target-or-angle)))]
      ;; Explicit target posn provided (e.g. player position)
      [(posn? target-or-angle)
       (define dx (- (posn-x target-or-angle) (posn-x enemy-pos)))
       (define dy (- (posn-y target-or-angle) (posn-y enemy-pos)))
       (define dist (sqrt (+ (sqr dx) (sqr dy))))
       (if (< dist 0.001)
           (values (* speed 0.707) (* speed 0.707))
           (values (* speed (/ dx dist))
                   (* speed (/ dy dist))))]
      ;; Default heuristic: aim diagonally downward towards nearest side wall
      [else
       (define ex (posn-x enemy-pos))
       ;; Left half -> fire down-left (3pi/4); Right half -> fire down-right (pi/4)
       (define angle (if (< ex (/ WIDTH 2)) (* 3/4 pi) (* 1/4 pi)))
       (values (* speed (cos angle))
               (* speed (sin angle)))]))
  (list (make-bouncing-projectile enemy-pos (velocity vx vy) bounces)))

;; Custom emitter pattern registry for modular extensibility
(define custom-emitters (make-hash))

(define (register-pattern-emitter! pattern-symbol emitter-proc)
  (hash-set! custom-emitters pattern-symbol emitter-proc))

;; Default pattern cycle
(define DEFAULT-ENEMY-PATTERN-CYCLE '(vertical radial aimed bouncing))

;; Advances pattern state for cyclic emitters
(define (advance-enemy-pattern pattern)
  (cond
    [(pair? pattern)
     (append (cdr pattern) (list (car pattern)))]
    [(eq? pattern 'cycle)
     '(radial aimed bouncing vertical)]
    [else pattern]))

;; Unified Pattern Dispatcher:
;; Resolves pattern descriptor (symbol, list of symbols, or custom procedure)
;; and emits the corresponding projectile list.
(define (emit-enemy-projectiles pattern enemy-pos player-pos)
  (define active-pattern
    (cond
      [(pair? pattern) (car pattern)]
      [(eq? pattern 'cycle) (car DEFAULT-ENEMY-PATTERN-CYCLE)]
      [else pattern]))
  (cond
    [(procedure? active-pattern)
     (active-pattern enemy-pos player-pos)]
    [(hash-ref custom-emitters active-pattern #f)
     => (λ (emitter) (emitter enemy-pos player-pos))]
    [else
     (case active-pattern
       [(vertical) (emit-vertical-bullets enemy-pos)]
       [(radial)   (emit-radial-bullets enemy-pos)]
       [(aimed)    (emit-aimed-bullets enemy-pos player-pos)]
       [(bouncing) (emit-bouncing-bullets enemy-pos player-pos)]
       [else       (emit-vertical-bullets enemy-pos)])]))

;; ====================================================================
;; Unit Tests
;; ====================================================================

(module+ test
  (require rackunit)

  (define ep (posn 300 100))
  (define pp (posn 300 500))

  ;; 1. Vertically falling bullet tests
  (define vb-list (emit-vertical-bullets ep 4))
  (check-equal? (length vb-list) 1 "Vertical emission creates 1 bullet")
  (define vb (car vb-list))
  (check-pred enemy-projectile? vb)
  (check-equal? (projectile-type vb) 'vertical)
  (check-equal? (projectile-velocity vb) (velocity 0 4))
  (define stepped-vb (step-projectile vb))
  (check-equal? (projectile-pos stepped-vb) (posn 300 104) "Moves vertically down")

  ;; 2. Radial bullet tests
  (define rb-list (emit-radial-bullets ep 8 4 0))
  (check-equal? (length rb-list) 8 "Radial emission creates 8 bullets")
  (for ([b (in-list rb-list)])
    (check-pred enemy-projectile? b)
    (check-equal? (projectile-type b) 'radial)
    (define v (projectile-velocity b))
    (define spd (sqrt (+ (sqr (velocity-x v)) (sqr (velocity-y v)))))
    (check-= spd 4.0 0.001 "Radial bullets have equal uniform speed"))

  ;; 3. Aimed bullet tests
  (define ab-list (emit-aimed-bullets ep pp 4))
  (check-equal? (length ab-list) 1 "Aimed emission creates 1 bullet")
  (define ab (car ab-list))
  (check-pred enemy-projectile? ab)
  (check-equal? (projectile-type ab) 'aimed)
  ;; Aiming directly below: dx=0, dy=400 -> vx=0, vy=4
  (check-= (velocity-x (projectile-velocity ab)) 0.0 0.001)
  (check-= (velocity-y (projectile-velocity ab)) 4.0 0.001)

  ;; Verify non-following: stepping ab does not change its fixed velocity even if player moves
  (define stepped-ab (step-projectile ab))
  (check-equal? (projectile-velocity stepped-ab) (projectile-velocity ab) "Aimed velocity remains fixed")

  ;; 4. Bouncing bullet tests
  ;; Bullet near left wall moving left with 1 bounce
  (define bb-init (make-bouncing-projectile (posn 6 100) (velocity -4 2) 1))
  (check-true (bouncing-projectile? bb-init))
  (check-equal? (projectile-bounces bb-init) 1)

  ;; Stepping hits left boundary (x <= 5) -> bounces: vx becomes positive, bounce count decrements to 0
  (define bb-bounced (step-projectile bb-init))
  (check-equal? (projectile-bounces bb-bounced) 0 "Bounce count decremented to 0")
  (check-true (> (velocity-x (projectile-velocity bb-bounced)) 0) "Horizontal velocity reflected")
  (check-equal? (velocity-y (projectile-velocity bb-bounced)) 2 "Vertical velocity preserved")

  ;; Stepping again with 0 bounces remaining moves linearly without further reflection
  (define bb-no-bounces (make-bouncing-projectile (posn 6 100) (velocity -4 2) 0))
  (define bb-after (step-projectile bb-no-bounces))
  (check-equal? (projectile-bounces bb-after) 0)
  (check-equal? (velocity-x (projectile-velocity bb-after)) -4 "Does not bounce when bounces=0")

  ;; 5. Pattern cycling
  (check-equal? (advance-enemy-pattern '(vertical radial aimed bouncing))
                '(radial aimed bouncing vertical))

  ;; 6. Unified emitter dispatch
  (check-equal? (length (emit-enemy-projectiles 'vertical ep pp)) 1)
  (check-equal? (length (emit-enemy-projectiles 'radial ep pp)) RADIAL-BULLET-COUNT)
  (check-equal? (length (emit-enemy-projectiles 'aimed ep pp)) 1)
  (check-equal? (length (emit-enemy-projectiles 'bouncing ep pp)) 1)

  ;; Custom procedural pattern dispatch
  (define (custom-pat e-pos p-pos)
    (list (make-vertical-projectile e-pos (velocity 1 1))
          (make-vertical-projectile e-pos (velocity -1 1))))
  (check-equal? (length (emit-enemy-projectiles custom-pat ep pp)) 2))
