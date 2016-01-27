#lang racket/gui

; Basic GL viewer with design loading and recompilation support.

(require (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 3)))

(provide gl-viewer%)

; The gl-viewer% widget provides our model-viewing facility with some niceties.
(define gl-viewer%
  (class canvas%
    (super-new)
    (inherit with-gl-context swap-gl-buffers)

    ; The draw and setup fields hold functions, given at initialization, that
    ; customize the widget's behavior.
    (init-field draw)
    (init-field (setup void))

    ; Has setup been called?  If false, the next on-paint event will call setup
    ; (with a valid GL context) before drawing.  The refresh event clears this
    ; back to false.
    (define setup-called #f)

    ; Flag used to coalesce queued low-priority refresh events.
    (define refresh-queued #f)

    ; Triggers a low-priority refresh event.  Display refresh events in Racket
    ; are normally higher priority than input events, which ensures that we'll
    ; get one refresh per e.g. mouse movement.  This is really bad if refresh is
    ; expensive.  This low-priority refresh mechanism lets us easily coalesce
    ; many input events into a single refresh.
    (define/public (low-priority-refresh)
      (unless refresh-queued
        (queue-callback (lambda ()
                          (send this refresh)
                          (set! refresh-queued #f))
                        #f)  ; <-- makes it low priority
        (set! refresh-queued #t)))

    ; Draw the scene.
    (define/overment (on-paint)
      (with-gl-context               
        (lambda ()
          ; Lazily call setup if required.
          (unless setup-called
            (setup)
            (set! setup-called #t))
          (glPushMatrix)
          (inner (void) on-paint)
          (glPopMatrix)
          (swap-gl-buffers))))

    (define/overment (on-char event)
      (case (send event get-key-code)
        [(f5) (set! setup-called #f) (low-priority-refresh)]
        [else (inner (void) on-char event)]))
    ))
