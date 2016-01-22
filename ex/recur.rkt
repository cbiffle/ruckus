; This is a somewhat ridiculous example demonstrating evaluating a distance
; field from within a design.
;
; The 'subordinate' design is compiled into a Racket distance field evaluator,
; which is then used to place and size a number of spheres.  The result
; approximates the subordinate design.
;
; With a GTX970 (driver 361.16) this takes ~5 seconds to compile and load, and
; gets single-digit frame rates.

(require racket)
(require "./core/compiler/racket.rkt")
(require "./core/math.rkt")
(require "./lang/evaluator.rkt")

(define threshold -10)

(define (design)
  (let ([df (node->distance-function (call-with-edsl-root subordinate))])
    (for ([i 40])
      (let* ([x (- (* 600 (random)) 300)]
             [y (- (* 600 (random)) 300)]
             [z (- (* 600 (random)) 300)]
             [f (df (vec3 x y z))])
        (when (< f threshold)
          (translate (list x y z) (sphere (- f))))))))

(define (subordinate) (rects 500 500 500))
