#lang racket

; Command line driver for 2D outline generation.
; 
; Output is in SVG.
;
; For command-line usage, see the end of this file or run with '--help'.

(require "marching-squares.rkt")
(require "loader.rkt")

; Knobs controlled from the command line, with default values:
(define design-size 128)    ; size of square ROI to consider
(define design-quantum 1)   ; size of quantum for terminal contour

(command-line
  #:program "outline"
  #:once-each
  [("-d" "--dimension") s
                        "Clip to <s> x <s> square around the origin."
                        (set! design-size (string->number s))]
  [("-q" "--quantum") q
                      "Quantize space into <q>-sized chunks."
                      (set! design-quantum (string->number q))]
  #:args (path)
  (begin
    (outline->svg (load-frep path) design-size design-quantum)))
