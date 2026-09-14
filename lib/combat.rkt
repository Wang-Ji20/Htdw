#lang racket/base

(require racket/math
         racket/list
         "config.rkt"
         "geometry.rkt"
         "entities.rkt")

(provide circles-intersect?
         projectile-hits-enemy?
         player-collides-enemy?
         player-collides-projectile?
         resolve-combat)

;; Intersection check between two circles defined by centers and radii
(define (circles-intersect? p1 r1 p2 r2)
  (< (distance-sqr p1 p2) (sqr (+ r1 r2))))

;; Checks if a projectile hits an enemy
(define (projectile-hits-enemy? proj e)
  (and (player-projectile? proj)
       (circles-intersect? (projectile-pos proj) PROJECTILE-RADIUS
                          (enemy-pos e) ENEMY-RADIUS)))

;; Checks if player collides with an enemy
(define (player-collides-enemy? p e)
  (circles-intersect? (player-pos p) PLAYER-RADIUS
                     (enemy-pos e) ENEMY-RADIUS))

;; Checks if player collides with an enemy projectile
(define (player-collides-projectile? p proj)
  (and (enemy-projectile? proj)
       (circles-intersect? (player-pos p) PLAYER-RADIUS
                          (projectile-pos proj) PROJECTILE-RADIUS)))

;; Pure functional collision resolution between projectiles and enemies.
;; Consumes colliding projectiles, applies damage to enemies, and tallies points.
;; Returns (values surviving-projectiles surviving-enemies points-earned)
(define (resolve-combat projectiles enemies)
  (define-values (remaining-projs remaining-enemies points-earned)
    (for/fold ([surviving-projs '()]
               [surviving-enemies enemies]
               [points 0])
              ([proj (in-list projectiles)])
      (if (player-projectile? proj)
          (let loop ([checked '()] [to-check surviving-enemies])
            (cond
              [(null? to-check)
               ;; Projectile did not hit any enemy; it survives
               (values (cons proj surviving-projs) checked points)]
              [(projectile-hits-enemy? proj (car to-check))
               ;; Hit an enemy! Consume projectile and destroy enemy
               (define hit-enemy (car to-check))
               (define next-hp (- (enemy-hp hit-enemy) 1))
               (define rest-enemies (cdr to-check))
               (if (<= next-hp 0)
                   ;; Enemy destroyed; award points
                   (values surviving-projs
                           (append checked rest-enemies)
                           (+ points 1))
                   ;; Enemy damaged but survives
                   (values surviving-projs
                           (cons (struct-copy enemy hit-enemy [hp next-hp])
                                 (append checked rest-enemies))
                           points))]
              [else
               (loop (cons (car to-check) checked) (cdr to-check))]))
          ;; Non-player projectile: preserve it
          (values (cons proj surviving-projs) surviving-enemies points))))
  (values (reverse remaining-projs) remaining-enemies points-earned))

;; ====================================================================
;; Unit Tests
;; ====================================================================

(module+ test
  (require rackunit)

  (define p0 (posn 100 100))
  (define p-close (posn 110 100))
  (define p-far (posn 200 200))

  (check-true (circles-intersect? p0 10 p-close 10))
  (check-false (circles-intersect? p0 10 p-far 10))

  (define proj (make-player-projectile p0 (velocity 0 -10)))
  (define enemy-hit (make-enemy p-close))
  (define enemy-miss (make-enemy p-far))

  (check-true (projectile-hits-enemy? proj enemy-hit))
  (check-false (projectile-hits-enemy? proj enemy-miss))

  (define-values (surv-p surv-e pts)
    (resolve-combat (list proj) (list enemy-hit enemy-miss)))
  (check-equal? surv-p '() "Hit projectile consumed")
  (check-equal? (length surv-e) 1 "Hit enemy removed")
  (check-equal? (car surv-e) enemy-miss "Missed enemy preserved")
  (check-equal? pts 1 "Points incremented"))
