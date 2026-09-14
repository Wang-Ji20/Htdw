#lang racket/base

(require racket/class
         racket/gui/base
         racket/draw
         "config.rkt"
         "world.rkt"
         "render.rkt")

(provide run)

(define (run)
  ;; Game State
  (define current-world (world-init))

  ;; Off-screen Double Buffer
  (define back-buffer (make-bitmap WIDTH HEIGHT))
  (define back-dc (new bitmap-dc% [bitmap back-buffer]))

  ;; Forward declaration for timer cleanup
  (define game-timer #f)

  ;; Top-level Frame with close handling
  (define game-frame%
    (class frame%
      (super-new)
      (define/augment (on-close)
        (when game-timer
          (send game-timer stop)))))

  (define frame
    (new game-frame%
         [label "Htdw - Danmaku Shooter"]
         [width WIDTH]
         [height HEIGHT]
         [style '(no-resize-border)]))

  ;; Custom Canvas to dispatch key events and double-buffered render
  (define game-canvas%
    (class canvas%
      (super-new)
      (inherit get-dc refresh)

      (define/override (on-paint)
        (render-world current-world back-dc)
        (send (get-dc) draw-bitmap back-buffer 0 0))

      (define/override (on-char event)
        (define code (send event get-key-code))
        (cond
          ;; Key Release
          [(eq? code 'release)
           (define rel-code (send event get-key-release-code))
           (set! current-world (world-key-up current-world rel-code))]

          ;; Restart Key when Game Over
          [(and (world-game-over? current-world)
                (or (eq? code #\r) (eq? code #\R) (eq? code #\return) (eq? code 'return)))
           (set! current-world (world-restart current-world))]

          ;; Regular Key Press
          [else
           (set! current-world (world-key-down current-world code))]))))

  (define canvas
    (new game-canvas%
         [parent frame]
         [min-width WIDTH]
         [min-height HEIGHT]
         [style '(no-autoclear)]))

  ;; Fixed 60 FPS Game Loop Timer
  (set! game-timer
        (new timer%
             [interval FRAME-TIME-MS]
             [notify-callback
              (λ ()
                (set! current-world (world-step current-world))
                (send canvas refresh))]))

  (send frame show #t)
  (send canvas focus))

(module+ main
  (run))
