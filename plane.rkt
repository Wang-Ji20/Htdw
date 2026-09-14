#lang racket/base

;; ====================================================================
;; Backward-Compatible Root Launcher & Facade
;; ====================================================================

(require "lib/config.rkt"
         "lib/geometry.rkt"
         "lib/entities.rkt"
         "lib/combat.rkt"
         "lib/world.rkt"
         "lib/render.rkt"
         "lib/main.rkt")

(provide (all-from-out "lib/config.rkt")
         (all-from-out "lib/geometry.rkt")
         (all-from-out "lib/entities.rkt")
         (all-from-out "lib/combat.rkt")
         (all-from-out "lib/world.rkt")
         (all-from-out "lib/render.rkt")
         (all-from-out "lib/main.rkt"))

(module+ main
  (run))

(module+ test
  (require rackunit
           (submod "lib/geometry.rkt" test)
           (submod "lib/combat.rkt" test)
           (submod "lib/world.rkt" test)
           (submod "lib/render.rkt" test)))
