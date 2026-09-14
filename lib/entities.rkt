#lang racket/base

(require "geometry.rkt")

(provide (struct-out player)
         (struct-out enemy)
         (struct-out projectile)
         make-player
         make-enemy
         make-player-projectile
         make-enemy-projectile
         player-projectile?
         enemy-projectile?)

;; Player entity
;; velocity : velocity
;; pos      : posn
;; cd       : non-negative integer (frames until next shot)
(struct player (velocity pos cd) #:transparent)

;; Enemy entity
;; velocity : velocity
;; pos      : posn
;; hp       : integer
(struct enemy (velocity pos hp) #:transparent)

;; Projectile entity
;; velocity : velocity
;; pos      : posn
;; emitter  : 'player | 'enemy
(struct projectile (velocity pos emitter) #:transparent)

;; Constructors
(define (make-player p [v (velocity 0 0)] [cd 0])
  (player v p cd))

(define (make-enemy p [v (velocity 0 0)] [hp 1])
  (enemy v p hp))

(define (make-player-projectile p v)
  (projectile v p 'player))

(define (make-enemy-projectile p v)
  (projectile v p 'enemy))

(define (player-projectile? proj)
  (eq? (projectile-emitter proj) 'player))

(define (enemy-projectile? proj)
  (eq? (projectile-emitter proj) 'enemy))
