#lang racket

; Generates a 2D outline of an object's contours at its intersection with the
; XY plane.  This is useful for e.g. laser-cutting 2D objects.
; 
; Output is in SVG.
;
; For command-line usage, see the end of this file or run with '--help'.

(require racket/flonum)
(require "edsl.rkt")
(require "compiler.rkt")
(require "math.rkt")
(require "loader.rkt")

(define (next-power-of-two x)
  (arithmetic-shift 1 (integer-length (- x 1))))

; ------------------------------------------------------------------------------
; Line segment representation.  We represent line segments like SVG does:
; using a start and end point.

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

; Produces a single SVG line segment from a segment structure.
(define (print-segment-as-svg-line out s)
  (fprintf out
           "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\"/>~n"
           (real->double-flonum (segment-sx s))
           (real->double-flonum (segment-sy s))
           (real->double-flonum (segment-ex s))
           (real->double-flonum (segment-ey s))))


; ------------------------------------------------------------------------------
; Outline projection and generation.

; Runs 'gen' as an EDSL root.  Evaluates the resulting geometry in the XY plane,
; focusing on a 'width' x 'height' sized rectangle centered around the origin.
; Geometry is diced up into 'granule'-sized squares and contours, if present,
; are emitted to the port 'out' as SVG.
;
; (The actual area evaluated will be rounded up to a square power of two, so
; don't use 'width' and 'height' in an attempt to clip geometry.)
(define (sparse-project-outline out gen width height granule)
  (let* ([side (next-power-of-two
                 (exact-ceiling ((max width height) . / . granule)))]
         [side/2 (side . / . 2)]
         [f (node->function (call-with-edsl-root gen))])
    (fprintf out "<?xml version=\"1.0\" standalone=\"no\"?>~n")
    (fprintf out "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"~n")
    (fprintf out "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">~n")
    (fprintf out "<svg version=\"1.1\"~n")
    (let ([vb-left/top (real->double-flonum (* granule (- side/2)))]
          [vb-width/height (real->double-flonum (* granule side))])
      (fprintf out "  width=\"~amm\" height=\"~amm\"~n"
               vb-width/height
               vb-width/height)
      (fprintf out "  viewBox=\"~a ~a ~a ~a\"~n"
               vb-left/top
               vb-left/top
               vb-width/height
               vb-width/height))
      (fprintf out "  xmlns=\"~a\">~n"
               "http://www.w3.org/2000/svg")
    (fprintf out "<g stroke=\"black\" stroke-width=\"0.1\">~n")
    (sparse-march f
                  (- side/2)
                  (- side/2)
                  side
                  granule
                  (lambda (seg) (print-segment-as-svg-line out seg)))
    (fprintf out "</g>~n")
    (fprintf out "</svg>~n")))

; Recursive sparse marching squares algorithm; implementation factor of
; 'sparse-project-outline', above.
;
; Using a quantum (square size) 'q', evaluates the distance field evaluator 'f'
; at points in the XY plane to discover contours.  Evaluation is limited to the
; square (measured in quanta) starting at ('x', 'y') and measuring 's' quanta
; on a side.  Contours are discoered as 'segment' structs; as each is found, the
; 'out' function is applied to it.
;
; This algorithm uses kd-tree-style binary space partitioning to avoid
; evaluating regions of space containing no contours.
; 
; The distance field evaluator 'f' must be Lipschitz continuous at L=1 for this
; to work correctly.
(define (sparse-march f x y s q out)
  (let* ([s/2 (s . / . 2)]
         ; Distance from sample point to corner in q units
         [dist-to-corner (sqrt (+ (sqr s/2) (sqr s/2)))]
         ; Distance from sample point to nearest surface, also in q units.
         ; Since 'f' is natively in world units we have to adjust.
         [dist-to-surface (/ (f (vec3 (* q (+ x s/2))
                                      (* q (+ y s/2))
                                      0))
                             q)])
    (when ((abs dist-to-surface) . <= . dist-to-corner)
      (if (= s 1)
        (for ([seg (in-list (marching-square f (* q x) (* q y) q))])
          (out (segment-scale (segment-offset seg x y) q)))
        (begin
          (sparse-march f x y s/2 q out)
          (sparse-march f (+ x s/2) y s/2 q out)
          (sparse-march f x (+ y s/2) s/2 q out)
          (sparse-march f (+ x s/2) (+ y s/2) s/2 q out))))))

; Implementation factor of 'marching-square', below.  Given the evaluated field
; levels at each of four corners of a square -- starting from the lower left and
; moving counter-clockwise -- this produces a numeric index to the contour table
; below.  (It is also, at the time of this writing, the same encoding used in
; the examples on Wikipedia -- so you can refer to that article.)
(define (corner-code a b c d)
  (define (bit v x) (if (< x 0) v 0))

  (+ (bit 1 a)
     (bit 2 b)
     (bit 4 c)
     (bit 8 d)))

; Single-square marching squares algorithm.  Produces zero, one, or two line
; segments in a list describing the isocontours of distance field evaluator 'f'
; within the square anchored at 'x' 'y' and measuring 'i' on a side.
(define (marching-square f x y i)
  ; Given two values, one above zero and one below, find the offset of the
  ; root between them.
  (define (span p q)
    ((0 . - . p) . / . (q . - . p)))

  (let* ([a (f (vec3    x       y    0))]
         [b (f (vec3 (+ x i)    y    0))]
         [c (f (vec3 (+ x i) (+ y i) 0))]
         [d (f (vec3    x    (+ y i) 0))]

         [code (corner-code a b c d)])
    (case code
      [(0) '()]
      [(1) (list (segment 0 (span a d)
                          (span a b) 0))]
      [(2) (list (segment (span a b) 0
                          1 (span b c)))]
      [(3) (list (segment 0 (span a d)
                          1 (span b c)))]
      [(4) (list (segment 1 (span b c)
                          (span d c) 1))]
      [(5)
       (let ([e (f (vec3 (+ x (/ i 2)) (+ y (/ i 2)) 0))])
         (if (<= e 0)
           (list (segment 0 (span a d)
                          (span d c) 1)
                 (segment 1 (span b c)
                          (span a b) 0))
           (list (segment 0 (span a d)
                          (span a b) 0)
                 (segment 1 (span b c)
                          (span d c) 1))))]

      [(6) (list (segment (span a b) 0
                          (span d c) 1))]
      [(7) (list (segment 0 (span a d)
                          (span d c) 1))]
      [(8) (list (segment (span d c) 1
                          0 (span a d)))]
      [(9) (list (segment (span d c) 1
                          (span a b) 0))]

      [(10)
       (let ([e (f (vec3 (+ x (/ i 2)) (+ y (/ i 2)) 0))])
         (if (<= e 0)
           (list (segment (span a b) 0
                          0 (span a d))
                 (segment (span d c) 1
                          1 (span b c)))
           (list (segment (span a b) 0
                          1 (span b c))
                 (segment (span d c) 1
                          0 (span a d)))))]

      [(11) (list (segment (span d c) 1
                           1 (span b c)))]
      [(12) (list (segment 1 (span b c)
                           0 (span a d)))]
      [(13) (list (segment 1 (span b c)
                           (span a b) 0))]
      [(14) (list (segment (span a b) 0
                           0 (span a d)))]
      [(15) '()])))


; ------------------------------------------------------------------------------
; Command line driver.

; Knobs controlled from the command line, with default values:
(define design-width 100)   ; width of rectangle to consider
(define design-height 100)  ; height of rectangle to consider
(define design-quantum 1)   ; size of quantum for marching squares

(command-line
  #:program "outline"
  #:once-each
  [("-d" "--dimension") w h
                        "Clip to <w> x <h> rectangle around the origin."
                        (begin
                          (set! design-width (string->number w))
                          (set! design-height (string->number h)))]
  [("-q" "--quantum") q
                      "Quantize space into <q>-sized chunks."
                      (set! design-quantum (string->number q))]
  #:args (path)
  (begin
    (sparse-project-outline (current-output-port)
                            (load-frep path)
                            design-width
                            design-height
                            design-quantum)))
