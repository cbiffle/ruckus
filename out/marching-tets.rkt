#lang racket

; Marching tetrahedra implementation, inspired by Paul Bourke's implementation:
; http://paulbourke.net/geometry/polygonise/
;
; The actual "marching" phase uses the same Lipschitz-gradient-guided traversal
; algorithm as my 2D outline extractor, with similar performance
; characteristics.

(provide surface->stl)

(require "../core/math.rkt")
(require "../core/compiler/racket.rkt")
(require "../lang/evaluator.rkt")

; ------------------------------------------------------------------------------
; Entry point.

; Takes an unevaluated design generator 'gen', a region of interest size
; 'size', and a quantum 'q' and generates STL for any surfaces within the ROI.
;
; Both 'size' and 'q' are given in the same (design) units.
;
; Output is to 'current-output-port'.
(define (surface->stl gen size q)
  ; The sparse algorithm requires power-of-two region sizes measured in units of
  ; 'q'.  Round the ROI size up to ensure this.
  (let* ([size2 (round-up-to-power-of-two (exact-ceiling (size . / . q)))]
         [size2/2 (size2 . / . 2)]
         [f (node->distance-function (call-with-edsl-root gen))])
    (sparse-polygonize-stl
      f
      (- size2/2)
      (- size2/2)
      (- size2/2)
      size2
      q)))


; ------------------------------------------------------------------------------
; STL generation.

; Wrapper for 'sparse-polygonize' (below) that produces binary STL to the
; 'current-output-port'.  (Which needs to be a file, because we'll try to seek
; it.)
(define (sparse-polygonize-stl f x y z side q)
  ; STL header.
  (write-bytes (make-bytes 80 32))
  ; Placeholder for number-of-triangles, TBD.
  (write-bytes (make-bytes 4 0))
  (define triangle-count 0)

  ; Polygonize...
  (sparse-polygonize f x y z side q
              (lambda (tri)
                ; Compute the cross product so we can find the face normal.
                (let* ([ba (vec3-sub (second tri) (first tri))]
                       [ca (vec3-sub (third tri) (first tri))]
                       [cross (vec3-cross ba ca)])
                  ; Omit degenerate triangles (zero-length cross).
                  (unless (zero? (vec3-length cross))
                    (set! triangle-count (add1 triangle-count))
                    ; Binary STL format consists of:
                    ; The normal vector
                    (stl-write-vector (vec3-normalize cross))
                    ; Three corner vectors
                    (for ([p (in-list tri)])
                      (stl-write-vector p))
                    ; A two-byte field that nobody can agree on.
                    (write-byte 0)
                    (write-byte 0)))))
  ; Return to the length and fill in our final tally.
  (file-position (current-output-port) 80)
  (write-bytes (integer->integer-bytes triangle-count 4 #f )))

(define (stl-write-vector v)
  (write-bytes (real->floating-point-bytes (vec3-x v) 4 #f))
  (write-bytes (real->floating-point-bytes (vec3-y v) 4 #f))
  (write-bytes (real->floating-point-bytes (vec3-z v) 4 #f)))


; ------------------------------------------------------------------------------
; Sparse descent algorithm.

; Divides space recursively into cubes, emitting triangles once the cubes reach
; one quantum 'q' in size.
;
; This algorithm assumes that 'f' is Lipschitz for L=1, meaning that it acts as
; a signed distance bound.  Given this, it can triangulate a surface of N
; triangles occupying V quanta of space in O(N log V) time.
;
; 'x', 'y', and 'z' give the "bottom" (most negative) corner of the cube being
; considered, on each axis.  These are in units of 'q'.
;
; 'side' is the length of any side of the cube, measured in units of 'q'.
;
; 'q' is the triangulation quantum, in design units.  Smaller values produce
; higher quality at cost roughly proportional to the inverse square.
;
; 'out-fn' is a function of one argument that will be called for each discovered
; triangle.
(define (sparse-polygonize f x y z side q out-fn)
  (if (= 1 side)
    ; Descent completed, run the basic algorithm.
    (polygonize-cube f (vec3-mul (vec3 x y z) q) q out-fn)
    ; Otherwise, we must descend further, iff the field level merits it.
    (let* ([s/2 (side . / . 2)]
           [center/q (vec3-add (vec3 x y z) s/2)]
           [center-to-corner/q (vec3-length (vec3 s/2 s/2 s/2))]
           [center (vec3-mul center/q q)]
           [center-to-corner (* q center-to-corner/q)]
           [center-level (f center)])
      (when ((abs center-level) . <= . center-to-corner)
        (sparse-polygonize f    x         y         z      s/2 q out-fn)
        (sparse-polygonize f (+ x s/2)    y         z      s/2 q out-fn)
        (sparse-polygonize f    x      (+ y s/2)    z      s/2 q out-fn)
        (sparse-polygonize f (+ x s/2) (+ y s/2)    z      s/2 q out-fn)
        (sparse-polygonize f    x         y      (+ z s/2) s/2 q out-fn)
        (sparse-polygonize f (+ x s/2)    y      (+ z s/2) s/2 q out-fn)
        (sparse-polygonize f    x      (+ y s/2) (+ z s/2) s/2 q out-fn)
        (sparse-polygonize f (+ x s/2) (+ y s/2) (+ z s/2) s/2 q out-fn)))))


; ------------------------------------------------------------------------------
; Cube division into tetrahedra and core triangulation routine.

; Describes a cube being triangulated.  'points' and 'values' are both vectors
; of equal length, containing the vertices and field values, respectively.
(struct grid-cell (points values) #:transparent)

; Convenience accessors for getting the point and value by index.
(define (g-point-ref gc idx) (vector-ref (grid-cell-points gc) idx))
(define (g-value-ref gc idx) (vector-ref (grid-cell-values gc) idx))

; Processes a cube with most negative vertex 'corner' and size 'size', both in
; design units.  Calls 'out-fn' with each triangle discovered.
(define (polygonize-cube f corner size out-fn)
  (let* ([corners (vector corner                                      ; 0
                          (vec3-add corner (vec3 size 0    0))        ; 1
                          (vec3-add corner (vec3 size size 0))        ; 2
                          (vec3-add corner (vec3 0    size 0))        ; 3
                          (vec3-add corner (vec3 0    0    size))     ; 4
                          (vec3-add corner (vec3 size 0    size))     ; 5
                          (vec3-add corner (vec3 size size size))     ; 6
                          (vec3-add corner (vec3 0    size size)))]   ; 7
         [levels (vector-map f corners)]
         [gc (grid-cell corners levels)])
    (polygonize-tet gc 0 2 3 7 out-fn)
    (polygonize-tet gc 0 7 6 2 out-fn)
    (polygonize-tet gc 0 4 6 7 out-fn)
    (polygonize-tet gc 0 6 1 2 out-fn)
    (polygonize-tet gc 0 6 4 1 out-fn)
    (polygonize-tet gc 5 6 1 4 out-fn)))

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

(define (occupied? val) (negative? val))

; Creates a triangle whose corners fall on the edges a-b, c-d, and e-f, given
; as vertex indices within the grid-cell 'g'.  The position of the vertices on
; each edge is determined by interpolation using the field values at each
; vertex, to approximate the location of the root.
(define ((verp-tri g) a b c d e f)
  (list (verp (g-point-ref g a) (g-point-ref g b)
              (g-value-ref g a) (g-value-ref g b))
        (verp (g-point-ref g c) (g-point-ref g d)
              (g-value-ref g c) (g-value-ref g d))
        (verp (g-point-ref g e) (g-point-ref g f)
              (g-value-ref g e) (g-value-ref g f))))

; Interpolates between two vertices using their field values.  'p1' and 'p2' are
; the coordinates of two vertices, in design units.  'val1' and 'val2' are the
; corresponding field values.
(define (verp p1 p2 val1 val2)
  (let ([mu (val1 . / . (val1 . - . val2))])
    (vec3-add p1 (vec3-mul (vec3-sub p2 p1) mu))))
