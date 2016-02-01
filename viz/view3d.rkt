#lang racket/gui

(require "../core/lazy-require.rkt")
(lazy-require "./spheretrace-viewer.rkt" spheretrace-viewer%)

; ------------------------------------------------------------------------------
; Command line interface.

(define (run)
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
      (printf "Loading opengl bindings...~n")
      (time spheretrace-viewer%)

      (printf "Creating viewer window...~n")
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

      (send frame show #t))))

(module+ main (run))
