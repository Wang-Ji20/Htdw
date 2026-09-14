#lang racket/base

(require racket/class
         racket/draw
         racket/list
         "config.rkt"
         "geometry.rkt"
         "entities.rkt"
         "world.rkt")

(provide render-world
         get-player-bitmap
         get-enemy-bitmap)

;; ====================================================================
;; Asset Loading & Drawing Primitives
;; ====================================================================

(define player-bitmap (read-bitmap PLAYER-SPRITE-PATH))
(define enemy-bitmap  (read-bitmap ENEMY-SPRITE-PATH))

(define (get-player-bitmap) player-bitmap)
(define (get-enemy-bitmap)  enemy-bitmap)

;; Brushes and Pens
(define bg-color (make-color 16 18 32))
(define bg-brush (new brush% [color bg-color] [style 'solid]))

(define bullet-brush (new brush% [color (make-color 255 60 60)] [style 'solid]))
(define bullet-pen   (new pen%   [color (make-color 255 200 100)] [width 1] [style 'solid]))
(define no-pen       (new pen%   [style 'transparent]))

(define hud-font (make-font #:size 16 #:family 'modern #:weight 'bold))
(define hud-color (make-color 255 220 50))

(define game-over-font (make-font #:size 36 #:family 'modern #:weight 'bold))
(define game-over-color (make-color 255 60 60))

(define score-font (make-font #:size 20 #:family 'modern #:weight 'bold))
(define score-color (make-color 255 255 255))

(define prompt-font (make-font #:size 14 #:family 'modern #:weight 'normal))
(define prompt-color (make-color 200 200 200))

(define overlay-brush (new brush% [color (make-color 0 0 0 0.75)] [style 'solid]))

;; ====================================================================
;; Rendering Engine
;; ====================================================================

;; Centers a bitmap on (cx, cy)
(define (draw-centered-bitmap dc bm cx cy)
  (define w (send bm get-width))
  (define h (send bm get-height))
  (send dc draw-bitmap bm (- cx (/ w 2)) (- cy (/ h 2))))

;; Draws a projectile
(define (draw-projectile dc proj)
  (define p (projectile-pos proj))
  (define x (posn-x p))
  (define y (posn-y p))
  (send dc set-brush bullet-brush)
  (send dc set-pen bullet-pen)
  (send dc draw-ellipse (- x PROJECTILE-RADIUS)
                        (- y PROJECTILE-RADIUS)
                        (* 2 PROJECTILE-RADIUS)
                        (* 2 PROJECTILE-RADIUS)))

;; Draws HUD with current score
(define (draw-hud dc points)
  (send dc set-font hud-font)
  (send dc set-text-foreground hud-color)
  (define text-str (format "SCORE: ~a" points))
  (define-values (tw th _1 _2) (send dc get-text-extent text-str))
  (send dc draw-text text-str (- (/ WIDTH 2) (/ tw 2)) 20))

;; Draws Game Over screen overlay
(define (draw-game-over dc points)
  ;; Dark overlay
  (send dc set-pen no-pen)
  (send dc set-brush overlay-brush)
  (send dc draw-rectangle 0 0 WIDTH HEIGHT)

  ;; Game Over Title
  (send dc set-font game-over-font)
  (send dc set-text-foreground game-over-color)
  (define title-str "GAME OVER")
  (define-values (tw th _1 _2) (send dc get-text-extent title-str))
  (send dc draw-text title-str (- (/ WIDTH 2) (/ tw 2)) (- (/ HEIGHT 2) 80))

  ;; Final Score
  (send dc set-font score-font)
  (send dc set-text-foreground score-color)
  (define score-str (format "Final Score: ~a" points))
  (define-values (sw sh _3 _4) (send dc get-text-extent score-str))
  (send dc draw-text score-str (- (/ WIDTH 2) (/ sw 2)) (- (/ HEIGHT 2) 20))

  ;; Restart prompt
  (send dc set-font prompt-font)
  (send dc set-text-foreground prompt-color)
  (define prompt-str "Press 'R' or ENTER to Restart")
  (define-values (pw ph _5 _6) (send dc get-text-extent prompt-str))
  (send dc draw-text prompt-str (- (/ WIDTH 2) (/ pw 2)) (+ (/ HEIGHT 2) 40)))

;; Renders the entire world to the provided drawing context
(define (render-world w dc)
  ;; 1. Clear background
  (send dc set-pen no-pen)
  (send dc set-brush bg-brush)
  (send dc draw-rectangle 0 0 WIDTH HEIGHT)

  ;; 2. Draw projectiles
  (for ([proj (in-list (world-projectiles w))])
    (draw-projectile dc proj))

  ;; 3. Draw enemies
  (for ([e (in-list (world-enemies w))])
    (define ep (enemy-pos e))
    (draw-centered-bitmap dc enemy-bitmap (posn-x ep) (posn-y ep)))

  ;; 4. Draw player
  (define pp (player-pos (world-player w)))
  (draw-centered-bitmap dc player-bitmap (posn-x pp) (posn-y pp))

  ;; 5. Draw HUD
  (draw-hud dc (world-points w))

  ;; 6. Draw Game Over Overlay if ended
  (when (world-game-over? w)
    (draw-game-over dc (world-points w))))

;; ====================================================================
;; Unit Tests (Offscreen Rendering)
;; ====================================================================

(module+ test
  (require rackunit)

  (define target-bm (make-bitmap WIDTH HEIGHT))
  (define test-dc (new bitmap-dc% [bitmap target-bm]))

  (define test-w (world-init))
  ;; Renders successfully to dc without crashing or throwing
  (check-not-exn (λ () (render-world test-w test-dc)))

  ;; Game over rendering
  (define dead-w (struct-copy world test-w [game-over? #t]))
  (check-not-exn (λ () (render-world dead-w test-dc))))
