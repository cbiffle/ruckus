#lang racket

; Marching tetrahedra implementation, inspired by Paul Bourke's implementation:
; http://paulbourke.net/geometry/polygonise/
;
; This doesn't contain the actual "marching" portion, only the code to generate
; triangles for a cube of space.

(provide marching-tets)

(require "../core/math.rkt")
(require "./marching-foo.rkt")

; Processes a cube with vertex locations and field values given by the grid-cell
; 'gc'.  Calls 'out-fn' with each triangle discovered.
(define (marching-tets gc out-fn)
  (polygonize-tet gc 0 2 3 7 out-fn)
  (polygonize-tet gc 0 7 6 2 out-fn)
  (polygonize-tet gc 0 4 6 7 out-fn)
  (polygonize-tet gc 0 6 1 2 out-fn)
  (polygonize-tet gc 0 6 4 1 out-fn)
  (polygonize-tet gc 5 6 1 4 out-fn))

; Finds zero, one, or two triangles that describe the contour of a sampled
; field within one tetrahedral division of a sampling cube.  The cube's corners
; and sample levels are defined by the grid-cell 'g'.  The subset of cube
; vertex indices making up the tetrahedron are given by 'v0' through 'v3'.
;
; Any triangles discovered are passed to invocations of 'out-fn' as the sole
; argument.
(define (polygonize-tet g v0 v1 v2 v3 out-fn)
  (define vg (verp-tri g))

  (let ([ind (triindex g v0 v1 v2 v3)])
    (define (out t)
      (if (ind . > . 7)
        (out-fn (reverse t))
        (out-fn t)))
    (case ind
      [(#x00 #x0F) void]
      [(#x01 #x0E)  ; occupied: 0   clear: 1,2,3
       (out (vg v0 v1
                v0 v2
                v0 v3))]
      [(#x02 #x0D)  ; occupied: 1   clear: 0,2,3
       (out (vg v1 v0
                v1 v3
                v1 v2))]
      [(#x03 #x0C)  ; occupied 0,1  clear: 2,3
       (out (vg v1 v3
                v0 v2
                v0 v3))
       (out (vg v1 v3
                v1 v2
                v0 v2))]
      [(#x04 #x0B)  ; occupied: 2   clear: 0,1,3
       (out (vg v2 v0
                v2 v1
                v2 v3))]
      [(#x05 #x0A)  ; occupied: 0,2   clear: 1,3
       (out (vg v0 v1
                v2 v3
                v0 v3))
       (out (vg v0 v1
                v1 v2
                v2 v3))]
      [(#x06 #x09)  ; occupied: 1,2   clear: 0,3
       (out (vg v0 v1
                v1 v3
                v2 v3))
       (out (vg v2 v3
                v0 v2
                v0 v1))]
      [(#x07 #x08)  ; occupied: 0,1,2   clear: 3
       (out (vg v3 v1
                v3 v2
                v3 v0))])))

; Translates field levels at four corners of a tetrahedron into a binary
; occupancy code.
(define (triindex g a b c d)
  (+ (if (occupied? (g-value-ref g a)) 1 0)
     (if (occupied? (g-value-ref g b)) 2 0)
     (if (occupied? (g-value-ref g c)) 4 0)
     (if (occupied? (g-value-ref g d)) 8 0)))

; Creates a triangle whose corners fall on the edges a-b, c-d, and e-f, given
; as vertex indices within the grid-cell 'g'.  The position of the vertices on
; each edge is determined by interpolation using the field values at each
; vertex, to approximate the location of the root.
(define ((verp-tri g) a b c d e f)
  (list (vec3-linear-root (g-point-ref g a) (g-value-ref g a)
                          (g-point-ref g b) (g-value-ref g b))
        (vec3-linear-root (g-point-ref g c) (g-value-ref g c)
                          (g-point-ref g d) (g-value-ref g d))
        (vec3-linear-root (g-point-ref g e) (g-value-ref g e)
                          (g-point-ref g f) (g-value-ref g f))))
