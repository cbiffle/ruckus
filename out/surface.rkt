#lang racket

; Command line driver for 3D surface generation.
; 
; Output is in STL (ASCII).
;
; For command-line usage, see the end of this file or run with '--help'.

(require "../lang/loader.rkt")
(require "./marching-tets.rkt")

; Knobs controlled from the command line, with default values:
(define design-size 128)    ; size of cubic ROI to consider
(define design-quantum 1)   ; size of quantum for terminal contour

(command-line
  #:program "surface"
  #:once-each
  [("-d" "--dimension") s
                        "Clip to <s>-unit cube around the origin."
                        (set! design-size (string->number s))]
  [("-q" "--quantum") q
                      "Quantize space into <q>-sized chunks."
                      (set! design-quantum (string->number q))]
  #:args (path)
  (begin
    (surface->stl (load-frep path) design-size design-quantum)))
