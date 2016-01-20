#lang racket

; Marching tetrahedra implementation, inspired by Paul Bourke's implementation:
; http://paulbourke.net/geometry/polygonise/
;
; The actual "marching" phase uses the same Lipschitz-gradient-guided traversal
; algorithm as my 2D outline extractor, with similar performance
; characteristics.

(provide
  surface->stl)

(require "../core/math.rkt")
(require "../core/compiler/racket.rkt")
(require "../lang/evaluator.rkt")

(define (surface->stl gen size q)
  (let* ([size2 (round-up-to-power-of-two (exact-ceiling (size . / . q)))]
         [size2/2 (size2 . / . 2)]
         [f (node->distance-function (call-with-edsl-root gen))])
    (sparse-polygonize-test
      f
      (- size2/2)
      (- size2/2)
      (- size2/2)
      size2
      q)))

(define (sparse-polygonize-test f x y z side q)
  (displayln "solid")
  (sparse-polygonize f x y z side q
              (lambda (tri)
                (let* ([ba (vec3-sub (second tri) (first tri))]
                       [ca (vec3-sub (third tri) (first tri))]
                       [cross (vec3-cross ba ca)])
                  (unless (zero? (vec3-length cross))
                    (stl-vertex-or-normal
                      "facet normal"
                      (vec3-normalize cross))
                    (printf "outer loop~n")
                    (for ([p (in-list tri)])
                      (stl-vertex-or-normal "vertex" p))
                    (printf "endloop~nendfacet~n")))))
  (displayln "endsolid"))

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

(define (polygonize-test f sx sy sz ex ey ez size)
  (displayln "solid")
  (polygonize f sx sy sz ex ey ez size
              (lambda (tri)
                (let* ([ba (vec3-sub (second tri) (first tri))]
                       [ca (vec3-sub (third tri) (first tri))]
                       [cross (vec3-cross ba ca)])
                  (unless (zero? (vec3-length cross))
                    (stl-vertex-or-normal
                      "facet normal"
                      (vec3-normalize cross))
                    (printf "outer loop~n")
                    (for ([p (in-list tri)])
                      (stl-vertex-or-normal "vertex" p))
                    (printf "endloop~nendfacet~n")))))
  (displayln "endsolid"))

(define (stl-vertex-or-normal kind v)
  (printf "~a ~a ~a ~a~n"
          kind
          (real->double-flonum (vec3-x v))
          (real->double-flonum (vec3-y v))
          (real->double-flonum (vec3-z v))))

(define (polygonize f sx sy sz ex ey ez size out-fn)
  (for* ([x (in-range sx ex size)]
         [y (in-range sy ey size)]
         [z (in-range sz ey size)])
    (polygonize-cube f (vec3 x y z) size out-fn)))

(struct grid-cell (points values) #:transparent)

(define (g-point-ref gc idx) (vector-ref (grid-cell-points gc) idx))
(define (g-value-ref gc idx) (vector-ref (grid-cell-values gc) idx))

(define (polygonize-cube f corner size out-fn)
  (let* ([corners (vector corner
                          (vec3-add corner (vec3 size 0 0))
                          (vec3-add corner (vec3 size size 0))
                          (vec3-add corner (vec3 0 size 0))
                          (vec3-add corner (vec3 0 0 size))
                          (vec3-add corner (vec3 size 0 size))
                          (vec3-add corner (vec3 size size size))
                          (vec3-add corner (vec3 0 size size)))]
         [levels (vector-map f corners)]
         [gc (grid-cell corners levels)])
    (polygonize-tet gc 0 2 3 7 out-fn)
    (polygonize-tet gc 0 7 6 2 out-fn)
    (polygonize-tet gc 0 4 6 7 out-fn)
    (polygonize-tet gc 0 6 1 2 out-fn)
    (polygonize-tet gc 0 6 4 1 out-fn)
    (polygonize-tet gc 5 6 1 4 out-fn)
    ))

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
      [(#x01 #x0E)
       (out (vg v0 v1
                v0 v2
                v0 v3))]
      [(#x02 #x0D)
       (out (vg v1 v0
                v1 v3
                v1 v2))]
      [(#x03 #x0C)
       (out (vg v1 v3
                v0 v2
                v0 v3))
       (out (vg v1 v3
                v1 v2
                v0 v2))]
      [(#x04 #x0B)
       (out (vg v2 v0
                v2 v1
                v2 v3))]
      [(#x05 #x0A)
       (out (vg v0 v1
                v2 v3
                v0 v3))
       (out (vg v0 v1
                v1 v2
                v2 v3))]
      [(#x06 #x09)
       (out (vg v0 v1
                v1 v3
                v2 v3))
       (out (vg v2 v3
                v0 v2
                v0 v1))]
      [(#x07 #x08)
       (out (vg v3 v1
                v3 v2
                v3 v0))])))

(define (triindex g a b c d)
  (+ (if (occupied? (g-value-ref g a)) 1 0)
     (if (occupied? (g-value-ref g b)) 2 0)
     (if (occupied? (g-value-ref g c)) 4 0)
     (if (occupied? (g-value-ref g d)) 8 0)))

(define (occupied? val) (negative? val))

(define ((verp-tri g) a b c d e f)
  (list (verp (g-point-ref g a) (g-point-ref g b)
              (g-value-ref g a) (g-value-ref g b))
        (verp (g-point-ref g c) (g-point-ref g d)
              (g-value-ref g c) (g-value-ref g d))
        (verp (g-point-ref g e) (g-point-ref g f)
              (g-value-ref g e) (g-value-ref g f))))

(define (verp p1 p2 val1 val2)
  (let ([mu ((0 . - . val1) . / . (val2 . - . val1))])
    (vec3-add p1 (vec3-mul (vec3-sub p2 p1) mu))))
