#lang racket/base

(require racket/runtime-path
         racket/math)

(provide (all-defined-out))

;; ====================================================================
;; Display & Timing Configuration
;; ====================================================================

(define WIDTH 600)
(define HEIGHT 800)
(define FPS 60)
(define FRAME-TIME-MS (exact-round (/ 1000 FPS))) ; ~16 ms per frame

;; ====================================================================
;; Player Configuration
;; ====================================================================

(define PLAYER-WIDTH 26)
(define PLAYER-HEIGHT 44)
(define PLAYER-RADIUS (/ (min PLAYER-WIDTH PLAYER-HEIGHT) 2)) ; 13
(define PLAYER-SPEED 6)                                        ; px per frame at 60 FPS
(define PLAYER-COOLDOWN 6)                                     ; frames between shots (~10 shots/sec)

;; ====================================================================
;; Enemy Configuration
;; ====================================================================

(define ENEMY-WIDTH 44)
(define ENEMY-HEIGHT 54)
(define ENEMY-RADIUS (/ (min ENEMY-WIDTH ENEMY-HEIGHT) 2)) ; 22
(define ENEMY-SPEED 4)                                     ; px per frame at 60 FPS
(define ENEMY-SPAWN-CD 90)                                 ; frames between spawns (1.5s at 60 FPS)
(define ENEMY-SPAWN-X 200)
(define ENEMY-SPAWN-Y 120)

;; ====================================================================
;; Projectile Configuration
;; ====================================================================

(define PROJECTILE-RADIUS 5)
(define PROJECTILE-SPEED 18) ; px per frame upwards

;; ====================================================================
;; Asset Paths
;; ====================================================================

(define-runtime-path ASSETS-DIR "../assets")
(define-runtime-path PLAYER-SPRITE-PATH "../assets/marisa.png")
(define-runtime-path ENEMY-SPRITE-PATH "../assets/cirno.png")

