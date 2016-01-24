#lang racket

; An implementation of the 'marching squares' algorithm for isocontour
; generation, with some optimizations for Lipschitz-continuous scalar fields.

(provide outline->svg)


(require "../core/math.rkt")
(require "../core/compiler/racket.rkt")
(require "../lang/evaluator.rkt")


; ------------------------------------------------------------------------------
; Occupancy test, centralized so it stays consistent.

(define (occupied? v)
  (negative? v))


; ------------------------------------------------------------------------------
; Representation of directed line segments, and operations upon them.
;
; A line segment is represented by two pairs of (x,y) coordinates, a start pair
; and an end pair.

(struct segment (sx sy ex ey) #:transparent)

; Offsets a segment by x and y values.
(define (segment-offset s x y)
   (segment (+ x (segment-sx s))
            (+ y (segment-sy s))
            (+ x (segment-ex s))
            (+ y (segment-ey s))))

; Scales all components of a segment by a ratio.
(define (segment-scale s x)
  (segment (* x (segment-sx s))
           (* x (segment-sy s))
           (* x (segment-ex s))
           (* x (segment-ey s))))

(define (segment-reverse s)
  (segment (segment-ex s)
           (segment-ey s)
           (segment-sx s)
           (segment-sy s)))

; ------------------------------------------------------------------------------
; Processing a single square using predetermined field values.  The location of
; the square in world-space is immaterial; the segments produced are on the unit
; square.  The placement of the samples:
;
;   d ------- c
;   |         |
;   |    e    |
;   |         |
;   a ------- b

; Generates a numeric corner code, mapping the occupancy status of each of the
; four corners to a bit in a four-bit integer.
(define (corner-code a b c d)
  (+ (if (occupied? a) 1 0)
     (if (occupied? b) 2 0)
     (if (occupied? c) 4 0)
     (if (occupied? d) 8 0)))

; Given two field samples, one above and one below zero, computes the position
; of the zero point between them on a scale from 0 (first point) to 1 (second).
(define (span p q)
  ((0 . - . p) . / . (q . - . p)))
  
; Generates zero, one, or two contours for the square described by the five
; sample points.
(define (contours-for-square a b c d e out-f)
  (contours-for-square-impl a b c d e (corner-code a b c d) out-f))

(define (contours-for-square-impl a b c d e cc out-f)
  (case cc
    [(0) void]
    [(1) (out-f (segment 0 (span a d)
                         (span a b) 0))]
    [(2) (out-f (segment (span a b) 0
                         1 (span b c)))]
    [(3) (out-f (segment 0 (span a d)
                         1 (span b c)))]
    [(4) (out-f (segment 1 (span b c) 
                         (span d c) 1))]
    [(5)
     (if (occupied? e)            
       (begin
         (out-f (segment 0 (span a d)
                         (span d c) 1))
         (out-f (segment 1 (span b c)
                         (span a b) 0)))
       (begin  ; not occupied
         (out-f (segment 0 (span a d)
                         (span a b) 0))
         (out-f (segment 1 (span b c)
                         (span d c) 1))))]

    [(6) (out-f (segment (span a b) 0
                         (span d c) 1))]
    [(7) (out-f (segment 0 (span a d)
                         (span d c) 1))]
    [else (contours-for-square-impl a b c d (- e) (bitwise-xor cc 15)
                                    (lambda (s) (out-f (segment-reverse s))))]))


; ------------------------------------------------------------------------------
; Algorithm for locating all contour segments of a certain size within a region
; of interest.
;
; This is similar to marching squares, but optimized for relatively sparse
; contours defined by signed distance bound functions -- that is, implicit
; surfaces whose potential function is Lipschitz-continuous for L=1.
;
; I have not completed a rigorous analysis of this algorithm, but empirically it
; scales in the number of segments emitted, *not* the size of the region of
; interest, which is critical.
;
; The algorithm is parameterized in terms of 'q', the quantum size.  Once space
; is subdivided into a square of size 'q', the traditional marching-squares
; contour finding method is applied to produce output.  This means that features
; smaller than 'q' may be lost.  So it goes.
;
; 'out-f' is called with each discovered contour segment as its single argument.

(define (find-contours q sdbf x y s out-f)
  (let* ([s/2 (s . / . 2)]
         ; Center sample point in world coordinates.
         [world-center (vec3 (* q (+ x s/2))
                             (* q (+ y s/2))
                             0)]
         ; Distance from center sample point to corner in units of 'q'.
         [dist-to-corner (sqrt (+ (sqr s/2) (sqr s/2)))]
         ; Lower bound of distance from center sample point to nearest surface,
         ; in world units.
         [center-sample (sdbf world-center)]
         ; Lower bound of distance in units of 'q'.
         [dist-to-surface (/ center-sample q)])
    (when ((abs dist-to-surface) . <= . dist-to-corner)
      ; This area of space is worth considering more closely.
      (if (= s 1)
        ; We are done subdividing space; look for segments.
        (contours-for-square
          (sdbf (vec3 (* q x) (* q y) 0))
          (sdbf (vec3 (* q (add1 x)) (* q y) 0))
          (sdbf (vec3 (* q (add1 x)) (* q (add1 y)) 0))
          (sdbf (vec3 (* q x) (* q (add1 y)) 0))
          center-sample
          (lambda (seg)
            (out-f (segment-scale (segment-offset seg x y) q))))
        ; Otherwise, we must descend further.
        (begin
          (find-contours q sdbf    x         y      s/2 out-f)
          (find-contours q sdbf (+ x s/2)    y      s/2 out-f)
          (find-contours q sdbf (+ x s/2) (+ y s/2) s/2 out-f)
          (find-contours q sdbf    x      (+ y s/2) s/2 out-f))))))


; ------------------------------------------------------------------------------
; SVG output and formatting.

; Generates an SVG document structure and formatted group for line segments
; generated by the execution of 'body'.  The 'unit' specifies the real-world
; interpretation of 'width' and 'height', and should be one of the units
; understood by SVG (such as mm).
(define (wrap-svg-document width height unit body)
  (printf "<?xml version=\"1.0\" standalone=\"no\"?>~n")
  (printf "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"~n")
  (printf "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">~n")
  (printf "<svg version=\"1.1\"~n")
  (printf "  width=\"~a~a\" height=\"~a~a\"~n"
          (real->double-flonum width) unit
          (real->double-flonum height) unit)
  (let ([left (- (width . / . 2))]
        [top  (- (height . / . 2))])
    (printf "  viewBox=\"~a ~a ~a ~a\"~n"
            (real->double-flonum left)
            (real->double-flonum top)
            (real->double-flonum width)
            (real->double-flonum height)))
  (printf "  xmlns=\"http://www.w3.org/2000/svg\">~n")
  (printf "<g stroke=\"black\" stroke-width=\"0.1\">~n")

  (body)

  (printf "</g></svg>~n"))

; Generates a single line segment.
(define (print-segment-as-svg-line s)
  (printf "  <line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\"/>~n"
          (real->double-flonum (segment-sx s))
          (real->double-flonum (segment-sy s))
          (real->double-flonum (segment-ex s))
          (real->double-flonum (segment-ey s))))

(define (outline->svg gen size unit q)
  (let* ([size2 (round-up-to-power-of-two (exact-ceiling (size . / . q)))]
         [f (node->distance-function (call-with-edsl-root gen))])
    (wrap-svg-document
      (size2 . * . q)
      (size2 . * . q)
      unit
      (thunk
        (find-contours
          q
          f
          (- (size2 . / . 2))
          (- (size2 . / . 2))
          size2
          print-segment-as-svg-line)))))
