#lang racket

; Command line driver for 3D surface generation.
; 
; Output is in STL.
;
; For command-line usage, see the end of this file or run with '--help'.

(require "../lang/loader.rkt")
(require "./binary-stl.rkt")
(require "./marching-tets.rkt")
(require "./marching-cubes.rkt")

; Knobs controlled from the command line, with default values:
(define design-size 1000)    ; size of cubic ROI to consider
(define design-quantum 1)   ; size of quantum for terminal contour
(define polygonizer marching-cubes)

(command-line
  #:program "surface"

  #:usage-help
  "Generates an STL triangulation of the surface of a design."

  #:help-labels "--- options controlling output quality ---"

  #:once-each
  [("-d" "--dimension")
   s
   ("Focus on a cube around the origin, <s> units on a"
    "side (default: 1000).  This is a hint to the algorithm, not a clipping"
    "region; parts of the design outside the cube may be included.")
   (set! design-size (string->number s))]
  [("-q" "--quantum")
   q
   ("Generate triangles no bigger than <q> units on"
    "any axis.  This controls output quality; smaller <q> values make for"
    "smoother surfaces, but also bigger files and more compute time.  In"
    "general, halving the <q> value increases processing time 4x.")
   (set! design-quantum (string->number q))]

  #:help-labels "--- triangulation methods ---"

  #:once-any
  [("-c" "--cubes")
   "Use marching cubes."
   (set! polygonizer marching-cubes)]
  [("-t" "--tets")
   "Use marching tetrahedra."
   (set! polygonizer marching-tets)]

  #:args (design-path output-path)
  (call-with-output-file output-path #:exists 'replace
    (lambda (f)
      (parameterize ([current-output-port f])
        (surface->stl (load-frep design-path)
                      design-size
                      design-quantum
                      polygonizer)))))
