#lang racket

(require racket/flonum)
(require "edsl.rkt")
(require "compiler.rkt")
(require "math.rkt")
(require "loader.rkt")

(struct segment (sx sy ex ey) #:transparent)

(define (segment-offset s x y)
  (segment (+ x (segment-sx s))
           (+ y (segment-sy s))
           (+ x (segment-ex s))
           (+ y (segment-ey s))))

(define (segment-scale s x)
  (segment (* x (segment-sx s))
           (* x (segment-sy s))
           (* x (segment-ex s))
           (* x (segment-ey s))))

(define (print-segment-as-svg-line out s)
  (fprintf out
           "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\"/>~n"
           (real->double-flonum (segment-sx s))
           (real->double-flonum (segment-sy s))
           (real->double-flonum (segment-ex s))
           (real->double-flonum (segment-ey s))))


(define (dense-project-outline out gen width height granule)
  (define (pd d)
    (real->double-flonum (* d granule)))

  (let ([half-width (width . / . 2)]
        [half-height (height . / . 2)]
        [diag-granule (granule . * . (sqrt 2))])
    (fprintf out "<?xml version=\"1.0\" standalone=\"no\"?>~n")
    (fprintf out "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"~n")
    (fprintf out "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">~n")
    (fprintf out "<svg viewBox=\"~a ~a ~a ~a\" xmlns=\"~a\" version=\"1.1\">~n"
             (- half-width)
             (- half-height)
             width
             height
             "http://www.w3.org/2000/svg")
    (fprintf out "<g stroke=\"black\" stroke-width=\"0.1\">~n")
    (let ([f (node->function (call-with-edsl-root gen))])
      (for* ([x (in-range (- half-width) half-width granule)]
             [y (in-range (- half-height) half-height granule)])
        (unless ((abs (f (vec3 x y 0))) . > . diag-granule)
          (for ([s (in-list (marching-squares f x y granule))])
            (fprintf out
                     "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\"/>~n"
                     (+ x (pd (segment-sx s)))
                     (+ y (pd (segment-sy s)))
                     (+ x (pd (segment-ex s)))
                     (+ y (pd (segment-ey s)))
                     )))))
    (fprintf out "</g>~n")
    (fprintf out "</svg>~n")))

(define (next-power-of-two x)
  (arithmetic-shift 1 (integer-length (- x 1))))

(define (sparse-project-outline out gen width height granule)
  (let* ([s (next-power-of-two
              (exact-ceiling ((max width height) . / . granule)))]
         [s/2 (s . / . 2)]
         [f (node->function (call-with-edsl-root gen))])
    (fprintf out "<?xml version=\"1.0\" standalone=\"no\"?>~n")
    (fprintf out "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"~n")
    (fprintf out "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">~n")
    (let ([vb-left/top (* granule (- s/2))]
          [vb-width/height (* granule s)])
      (fprintf out
               "<svg viewBox=\"~a ~a ~a ~a\" xmlns=\"~a\" version=\"1.1\">~n"
               vb-left/top
               vb-left/top
               vb-width/height
               vb-width/height
               "http://www.w3.org/2000/svg"))
    (fprintf out "<g stroke=\"black\" stroke-width=\"0.1\">~n")
    (sparse-march f
                  (- s/2)
                  (- s/2)
                  s
                  granule
                  (lambda (seg) (print-segment-as-svg-line out seg)))
    (fprintf out "</g>~n")
    (fprintf out "</svg>~n")
    ))

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
        (for ([seg (in-list (marching-squares f (* q x) (* q y) q))])
          (out (segment-scale (segment-offset seg x y) q)))
        (begin
          (sparse-march f x y s/2 q out)
          (sparse-march f (+ x s/2) y s/2 q out)
          (sparse-march f x (+ y s/2) s/2 q out)
          (sparse-march f (+ x s/2) (+ y s/2) s/2 q out))))))


(define (marching-squares f x y i)
  (segments f x y i))

(define (corner-code a b c d)
  (define (bit v x) (if (< x 0) v 0))

  (+ (bit 1 a)
     (bit 2 b)
     (bit 4 c)
     (bit 8 d)))

(define (segments f x y i)
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

(define design-width 100)
(define design-height 100)
(define design-quantum 1)

(define (outline path)
  (sparse-project-outline (current-output-port)
                   (load-frep path)
                   design-width
                   design-height
                   design-quantum))

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
  (outline path))

