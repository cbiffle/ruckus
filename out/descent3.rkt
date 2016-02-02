#lang racket

; Algorithms for subdividing a region of interest into fixed-size cubes for
; triangulation.

(provide
  lipschitz-subdivide
  exhaustive-subdivide)

(require "../core/math.rkt")
(require "../core/compiler/racket.rkt")
(require "../lang/evaluator.rkt")
(require "./marching-foo.rkt")


; ------------------------------------------------------------------------------
; Sparse descent algorithm.  Divides space recursively into cubes, emitting
; triangles once the cubes reach one quantum 'q' in size.
;
; This algorithm assumes that 'f' is Lipschitz for L=1, meaning that it acts as
; a signed distance bound.  Given this, it can triangulate a surface of N
; triangles occupying V quanta of space in O(N log V) time, which in practice is
; effective O(N).
;
; 'size' gives the minimum size of the ROI cube in design units, centered around
; the origin.  This will be rounded up to the next power of two.
;
; 'q' gives the target size of the triangulation cube, in design units.
;
; 'polygonize' is a function that will be called with a 'grid-cell' for each
; potentially occupied cube of space.
(define (lipschitz-subdivide f size q polygonize)
  (let* ([size2 (round-up-to-power-of-two (exact-ceiling (size . / . q)))]
         [-size2/2 (- (size2 . / . 2))])
    (sparse-polygonize f -size2/2 -size2/2 -size2/2 size2 q polygonize)))

; Implementation factor of lipschitz-subdivide.
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
(define (sparse-polygonize f x y z side q polygonize)
  (if (= 1 side)
    ; Descent completed, process the cube.
    (polygonize (make-grid-cell f
                                (vec3-mul q (vec3 x y z))
                                q))
    ; Otherwise, we must descend further, iff the field level merits it.
    (let* ([s/2 (side . / . 2)]
           [center/q (vec3-add (vec3 x y z) s/2)]
           [center-to-corner/q (vec3-length (vec3 s/2 s/2 s/2))]
           [center (vec3-mul center/q q)]
           [center-to-corner (* q center-to-corner/q)]
           [center-level (f center)])
      (when ((abs center-level) . <= . center-to-corner)
        (sparse-polygonize f    x         y         z      s/2 q polygonize)
        (sparse-polygonize f (+ x s/2)    y         z      s/2 q polygonize)
        (sparse-polygonize f    x      (+ y s/2)    z      s/2 q polygonize)
        (sparse-polygonize f (+ x s/2) (+ y s/2)    z      s/2 q polygonize)
        (sparse-polygonize f    x         y      (+ z s/2) s/2 q polygonize)
        (sparse-polygonize f (+ x s/2)    y      (+ z s/2) s/2 q polygonize)
        (sparse-polygonize f    x      (+ y s/2) (+ z s/2) s/2 q polygonize)
        (sparse-polygonize f (+ x s/2) (+ y s/2) (+ z s/2) s/2 q polygonize)
        (void)))))


; ------------------------------------------------------------------------------
; Brute force descent algorithm.  Does nothing clever; evaluates every cube of
; space.
;
; This algorithm takes O(size^3) time regardless of geometry, not counting time
; taken by 'polygonize'.  This means the Lipschitz sparse version is almost
; always faster.  However, this exhaustive version will work for *any* implicit
; surface, even if it is not Lipschitz, and can be a valuable tool.
;
; 'f' is a function from vec3 to real.
;
; 'size' is the size of the ROI cube in design units, centered around the
; origin.  It will be rounded up to the next multiple of 'q'.
;
; 'q' gives the target size of the triangulation cube, in design units.
;
; 'polygonize' is a function that will be called with a 'grid-cell' for each
; cube of space.
(define (exhaustive-subdivide f size q polygonize)
  (let* ([s (exact-ceiling (size . / . q))]
         [s/2 (s . / . 2)]
         [-s/2 (- (s . / . 2))])
    (for* ([x (in-range -s/2 s/2)]
           [y (in-range -s/2 s/2)]
           [z (in-range -s/2 s/2)])
      (polygonize (make-grid-cell f
                                  (vec3-mul q (vec3 x y z))
                                  q)))
    (void)))
