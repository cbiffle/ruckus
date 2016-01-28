#lang racket/gui

(require racket/runtime-path)

(require (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 4)))
(require ffi/vector)

(require "../core/math.rkt")
(require "../core/model.rkt")
(require "./glsl.rkt")
(require "./spheretrace-viewer.rkt")

; ------------------------------------------------------------------------------
; Command line interface.

(command-line
  #:program "ruckus-3d"

  #:usage-help
  "Displays a design interactively using ray-tracing."

  #:ps
  ""
  "Keyboard/mouse usage:"
  "- Drag with the mouse to rotate the design."
  "- Scroll wheel / gesture zooms."
  "- F5 reloads and recompiles any changes to the design."

  #:args (design-path)
  (begin
    (define frame
      (new frame%
           [label "Ruckus 3D"]
           [width 300]
           [height 300]))

    (define v
      (new spheretrace-viewer%
           [style '(gl no-autoclear)]
           [parent frame]
           [design-path design-path]))

    (send frame show #t)))
