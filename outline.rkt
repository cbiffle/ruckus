#lang racket

(require racket/flonum)
(require "edsl.rkt")
(require "compiler.rkt")
(require "math.rkt")
(require "loader.rkt")

(struct segment (sx sy ex ey) #:transparent)

(define (project-outline out gen width height granule)
  (define (pd d)
    (real->double-flonum (* d granule)))

  (let ([half-width (width . / . 2)]
        [half-height (height . / . 2)])
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
        (for ([s (in-list (marching-squares f x y granule))])
          (fprintf out
                   "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\"/>~n"
                   (+ x (pd (segment-sx s)))
                   (+ y (pd (segment-sy s)))
                   (+ x (pd (segment-ex s)))
                   (+ y (pd (segment-ey s)))
                   ))))
    (fprintf out "</g>~n")
    (fprintf out "</svg>~n")))

(define (marching-squares f x y i)
  (segments f x y i))

(define (corner-code a b c d)
  (define (bit v x) (if (<= x 0) v 0))

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
  (project-outline (current-output-port)
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

