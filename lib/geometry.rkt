#lang racket/base

(require racket/math)

(provide (struct-out posn)
         (struct-out velocity)
         vec+
         vec-
         posn+vec
         distance-sqr
         distance
         make-clamper
         in-bounds?)

;; 2D spatial coordinate
(struct posn (x y) #:transparent)

;; 2D vector / displacement
(struct velocity (x y) #:transparent)

;; Add two velocity vectors
(define (vec+ v1 v2)
  (velocity (+ (velocity-x v1) (velocity-x v2))
            (+ (velocity-y v1) (velocity-y v2))))

;; Subtract two velocity vectors
(define (vec- v1 v2)
  (velocity (- (velocity-x v1) (velocity-x v2))
            (- (velocity-y v1) (velocity-y v2))))

;; Translate a posn by a velocity vector
(define (posn+vec p v)
  (posn (+ (posn-x p) (velocity-x v))
        (+ (posn-y p) (velocity-y v))))

;; Squared Euclidean distance between two posn points
(define (distance-sqr p1 p2)
  (+ (sqr (- (posn-x p1) (posn-x p2)))
     (sqr (- (posn-y p1) (posn-y p2)))))

;; Euclidean distance between two posn points
(define (distance p1 p2)
  (sqrt (distance-sqr p1 p2)))

;; Creates a clamping function bounded by [cmin, cmax]
(define ((make-clamper cmin cmax) x)
  (cond
    [(< x cmin) cmin]
    [(> x cmax) cmax]
    [else x]))

;; Checks if a posn is within [min-x, max-x] and [min-y, max-y]
(define (in-bounds? p min-x min-y max-x max-y)
  (define x (posn-x p))
  (define y (posn-y p))
  (and (<= min-x x max-x)
       (<= min-y y max-y)))

;; ====================================================================
;; Unit Tests
;; ====================================================================

(module+ test
  (require rackunit)

  (check-equal? (vec+ (velocity 1 2) (velocity 3 4)) (velocity 4 6))
  (check-equal? (vec- (velocity 5 7) (velocity 2 3)) (velocity 3 4))
  (check-equal? (posn+vec (posn 10 20) (velocity 5 -2)) (posn 15 18))

  (check-equal? (distance-sqr (posn 0 0) (posn 3 4)) 25)
  (check-equal? (distance (posn 0 0) (posn 3 4)) 5)

  (define clamp (make-clamper 10 50))
  (check-equal? (clamp 5) 10)
  (check-equal? (clamp 25) 25)
  (check-equal? (clamp 100) 50)

  (check-true (in-bounds? (posn 50 50) 0 0 100 100))
  (check-false (in-bounds? (posn -5 50) 0 0 100 100))
  (check-false (in-bounds? (posn 50 150) 0 0 100 100)))
