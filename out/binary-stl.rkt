#lang racket

; Binary STL output.

(provide surface->stl)

(require "../core/math.rkt")
(require "../core/compiler/racket.rkt")
(require "../lang/evaluator.rkt")

; Takes an unevaluated design generator 'gen', a region of interest size
; 'size', and a quantum 'q' and generates STL for any surfaces within the ROI.
;
; The function 'subdivide' will be used to subdivide space into 'q'-sized cubes.
;
; The function 'polygonize' will be used to
;
; Both 'size' and 'q' are given in the same (design) units.
;
; Output is to 'current-output-port'.
(define (surface->stl gen size q subdivide polygonize)
  (let ([f (node->distance-function (call-with-edsl-root gen))])
    ; STL header.
    (write-bytes (make-bytes 80 32))
    ; Placeholder for number-of-triangles, TBD.
    (write-bytes (make-bytes 4 0))

    (define triangle-count 0)

    (define (emit tri)
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
          (write-byte 0))))

    (subdivide f size q
               (lambda (gc) (polygonize gc emit)))

    ; Return to the length and fill in our final tally.
    (file-position (current-output-port) 80)
    (write-bytes (integer->integer-bytes triangle-count 4 #f ))
    (void)))

(define (stl-write-vector v)
  (write-bytes (real->floating-point-bytes (vec3-x v) 4 #f))
  (write-bytes (real->floating-point-bytes (vec3-y v) 4 #f))
  (write-bytes (real->floating-point-bytes (vec3-z v) 4 #f))
  (void))
