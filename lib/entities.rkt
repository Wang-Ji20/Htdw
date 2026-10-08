#lang racket/base

(require "geometry.rkt"
         "config.rkt"
         "projectiles.rkt")

(provide (struct-out player)
         (struct-out enemy)
         make-player
         make-enemy
         (all-from-out "projectiles.rkt"))

;; ====================================================================
;; Player Entity
;; ====================================================================
;; velocity : velocity
;; pos      : posn
;; cd       : non-negative integer (frames until next shot)
(struct player (velocity pos cd) #:transparent)

(define (make-player p [v (velocity 0 0)] [cd 0])
  (player v p cd))

;; ====================================================================
;; Enemy Entity
;; ====================================================================
;; velocity : velocity
;; pos      : posn
;; hp       : integer
;; shoot-cd : non-negative integer (frames until next shot)
;; pattern  : pattern-descriptor (symbol, list of symbols, or custom procedure)
(struct enemy (velocity pos hp shoot-cd pattern) #:transparent)

(define (make-enemy p
                    [v (velocity 0 0)]
                    [hp 1]
                    [shoot-cd ENEMY-SHOOT-CD]
                    [pattern DEFAULT-ENEMY-PATTERN-CYCLE])
  (enemy v p hp shoot-cd pattern))
