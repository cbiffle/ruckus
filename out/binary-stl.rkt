#lang racket

; Binary STL output using Lipschitz-gradient-guided traversal and an unspecified
; local surface extraction routine.

(provide surface->stl)

(require "../core/math.rkt")
(require "../core/compiler/racket.rkt")
(require "../lang/evaluator.rkt")

; ------------------------------------------------------------------------------
; Entry point.

; Takes an unevaluated design generator 'gen', a region of interest size
; 'size', and a quantum 'q' and generates STL for any surfaces within the ROI.
;
; The function 'polygonize' will be called on each 'q'-sized cube believed to
; contain a fragment of surface.
;
; Both 'size' and 'q' are given in the same (design) units.
;
; Output is to 'current-output-port'.
(define (surface->stl gen size q polygonize)
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
      q
      polygonize)))


; ------------------------------------------------------------------------------
; STL generation.

; Wrapper for 'sparse-polygonize' (below) that produces binary STL to the
; 'current-output-port'.  (Which needs to be a file, because we'll try to seek
; it.)
(define (sparse-polygonize-stl f x y z side q polygonize)
  ; STL header.
  (write-bytes (make-bytes 80 32))
  ; Placeholder for number-of-triangles, TBD.
  (write-bytes (make-bytes 4 0))
  (define triangle-count 0)

  ; Polygonize...
  (sparse-polygonize f x y z side q polygonize
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
  (write-bytes (integer->integer-bytes triangle-count 4 #f ))
  (void))

(define (stl-write-vector v)
  (write-bytes (real->floating-point-bytes (vec3-x v) 4 #f))
  (write-bytes (real->floating-point-bytes (vec3-y v) 4 #f))
  (write-bytes (real->floating-point-bytes (vec3-z v) 4 #f))
  (void))


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
(define (sparse-polygonize f x y z side q pgon out-fn)
  (if (= 1 side)
    ; Descent completed, run the basic algorithm.
    (pgon f (vec3-mul (vec3 x y z) q) q out-fn)
    ; Otherwise, we must descend further, iff the field level merits it.
    (let* ([s/2 (side . / . 2)]
           [center/q (vec3-add (vec3 x y z) s/2)]
           [center-to-corner/q (vec3-length (vec3 s/2 s/2 s/2))]
           [center (vec3-mul center/q q)]
           [center-to-corner (* q center-to-corner/q)]
           [center-level (f center)])
      (when ((abs center-level) . <= . center-to-corner)
        (sparse-polygonize f    x         y         z      s/2 q pgon out-fn)
        (sparse-polygonize f (+ x s/2)    y         z      s/2 q pgon out-fn)
        (sparse-polygonize f    x      (+ y s/2)    z      s/2 q pgon out-fn)
        (sparse-polygonize f (+ x s/2) (+ y s/2)    z      s/2 q pgon out-fn)
        (sparse-polygonize f    x         y      (+ z s/2) s/2 q pgon out-fn)
        (sparse-polygonize f (+ x s/2)    y      (+ z s/2) s/2 q pgon out-fn)
        (sparse-polygonize f    x      (+ y s/2) (+ z s/2) s/2 q pgon out-fn)
        (sparse-polygonize f (+ x s/2) (+ y s/2) (+ z s/2) s/2 q pgon out-fn)
        ))))
