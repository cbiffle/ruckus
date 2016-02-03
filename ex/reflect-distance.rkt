#lang ruckus

(require ruckus/core/math)

; This (somewhat contrived) example demonstrates the use of reflect-distance
; by surrounding a cube by spheres that graze its faces.  The design reflects
; on one of its parts to determine the geometry of other parts, a limited form
; of constraint-based design.

(define (the-cube)
  (cube 300))

(define (sphere-cage spacing)
  (define distance-to-cube-from (reflect-distance (the-cube)))

  (for* ([x '(-1 0 1)]
         [y '(-1 0 1)]
         [z '(-1 0 1)])
    (let ([coord (vec3 (* x spacing) (* y spacing) (* z spacing))])
      (at coord
          (sphere #:radius (distance-to-cube-from coord))))))

(the-cube)
(color 'red (sphere-cage 200))
