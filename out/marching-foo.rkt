#lang racket

; Support code common to the implementations of Marching Cubes and Marching
; Tets.

(provide
  (struct-out grid-cell)
  g-point-ref
  g-value-ref
  make-grid-cell
  occupied?)

(require "../core/math.rkt")

; Describes a cube being triangulated.  'points' and 'values' are both vectors
; of equal length, containing the vertices and field values, respectively.
(struct grid-cell (points values) #:transparent)

; Convenience accessors for getting the point and value by index.
(define (g-point-ref gc idx) (vector-ref (grid-cell-points gc) idx))
(define (g-value-ref gc idx) (vector-ref (grid-cell-values gc) idx))

; Generates a grid cell for a given corner and cell size by evaluating the
; function 'f' at each corner.
(define (make-grid-cell f corner size)
  (let* ([corners (vector corner                                      ; 0
                          (vec3-add corner (vec3 size 0    0))        ; 1
                          (vec3-add corner (vec3 size size 0))        ; 2
                          (vec3-add corner (vec3 0    size 0))        ; 3
                          (vec3-add corner (vec3 0    0    size))     ; 4
                          (vec3-add corner (vec3 size 0    size))     ; 5
                          (vec3-add corner (vec3 size size size))     ; 6
                          (vec3-add corner (vec3 0    size size)))]   ; 7
         [levels (vector-map f corners)])
    (grid-cell corners levels)))

; Centralized occupancy test, so I don't get it backwards in some places.
(define (occupied? v) (negative? v))
