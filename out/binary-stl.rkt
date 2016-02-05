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
      (when (write-stl-tri tri)
        (set! triangle-count (add1 triangle-count))))

    (subdivide f size q
               (lambda (gc) (polygonize gc emit)))

    ; Return to the length and fill in our final tally.
    (file-position (current-output-port) 80)
    (write-bytes (integer->integer-bytes triangle-count 4 #f ))
    (void)))

; Writes the triangle 'tri' (a 3-list of vec3) to current-output-port as a
; binary STL triangle record, iff it is not degenerate.
;
; If the record is written, the result is #t.  If not, it is #f.
(define (write-stl-tri tri)
  ; Compute the cross product so we can find the face normal.
  (let* ([ba (vec3-sub (second tri) (first tri))]
         [ca (vec3-sub (third tri) (first tri))]
         [cross (vec3-cross ba ca)])
    ; Omit degenerate triangles (zero-length cross).
    (if (zero? (vec3-length cross))
      #f
      ; Generate and write a binary-format record.  Binary format records
      ; consist of:
      ; - The normal vector (12 bytes)
      ; - The triangle vertices (3 * 12 bytes)
      ; - Two trailing bytes upon which nobody agrees (2 bytes)
      (let ([buf (make-bytes (+ 12 (* 3 12) 2))])
        (vector->stl-bytes (vec3-normalize cross) buf 0)
        ; Three corner vectors
        (for ([p (in-list tri)]
              [off (in-range 12 48 12)])
          (vector->stl-bytes p buf off))
        (write-bytes buf)
        #t))))

(define (vector->stl-bytes v buf start)
  (real->floating-point-bytes (vec3-x v) 4 #f buf (+ start 0))
  (real->floating-point-bytes (vec3-y v) 4 #f buf (+ start 4))
  (real->floating-point-bytes (vec3-z v) 4 #f buf (+ start 8)))
