#lang racket/gui

(require "../core/lazy-require.rkt")
(lazy-require "./two-viewer.rkt" two-viewer%)

; ------------------------------------------------------------------------------
; Command line interface.

(define (run)
  (command-line
    #:program "ruckus-2d"

    #:usage-help
    "Displays a design interactively in 2D mode."

    #:ps
    ""
    "Keyboard/mouse usage:"
    "- Scroll wheel / gesture zooms."
    "- F5 reloads and recompiles any changes to the design."

    #:args (design-path)
    (begin
      (printf "Loading opengl bindings...~n")
      (time two-viewer%)

      (printf "Creating viewer window...~n")
      (define frame
        (new frame%
             [label "Ruckus 2D"]
             [width 300]
             [height 300]))

      (define v
        (new two-viewer%
             [style '(gl no-autoclear)]
             [parent frame]
             [design-path design-path]))

      (send frame show #t))))

(module+ main (run))
