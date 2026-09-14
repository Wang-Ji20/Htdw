#lang racket
(require 2htdp/image)
(define player-sprite (bitmap "./marisa.png"))
(define enemy-sprite (bitmap "./cirno.png"))
(printf "Success! Sizes: ~ax~a and ~ax~a\n"
        (image-width player-sprite) (image-height player-sprite)
        (image-width enemy-sprite) (image-height enemy-sprite))

