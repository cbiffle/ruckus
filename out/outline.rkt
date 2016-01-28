#lang racket

; Command line driver for 2D outline generation.
; 
; Output is in SVG.
;
; For command-line usage, see the end of this file or run with '--help'.

(require "../lang/loader.rkt")
(require "./marching-squares.rkt")

; Knobs controlled from the command line, with default values:
(define design-size 128)    ; size of square ROI to consider
(define design-quantum 1)   ; size of quantum for terminal contour
(define design-unit 'mm)    ; real-world mapping of design units

(command-line
  #:program "ruckus-export-outline"

  #:usage-help
  "Generates an SVG outline of the intersection of a design with the XY plane."

  #:help-labels "--- options controlling output quality ---"

  #:once-each
  [("-d" "--dimension")
   s
   ("Focus on a square around the origin, <s> units on a"
    "side.  This is a hint to the algorithm, not a clipping region; parts of"
    "the design outside this square may be included in the output.")
   (set! design-size (string->number s))]
  [("-q" "--quantum")
   q
   ("Generate line segments no bigger than <q> units on"
    "either axis.  This controls output quality; smaller <q> values make for"
    "smoother curves, but also bigger files and more compute time.  In general,"
    "halving the <q> value doubles processing time.")
   (set! design-quantum (string->number q))]

  #:help-labels "--- real-world unit selection ---"

  #:once-any
  [("--mm") "Design uses millimeter units (default)."
            (set! design-unit 'mm)]
  [("--cm") "Design uses centimeter units."
            (set! design-unit 'cm)]
  [("--inch") "Design uses inch units."
              (set! design-unit 'in)]
  [("--pt") "Design uses typographical point units (1/72 inch)."
            (set! design-unit 'pt)]
  [("--px") "Design uses pixel units (no real meaning)."
            (set! design-unit 'px)]

  #:args (design-path output-path)
  (call-with-output-file output-path #:exists 'replace
    (lambda (f)
      (parameterize ([current-output-port f])
        (outline->svg (load-frep design-path)
                      design-size
                      design-unit
                      design-quantum)))))
