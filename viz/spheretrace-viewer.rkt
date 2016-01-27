#lang racket/gui

; Extension of the basic GL viewer with features to support 3D spheretracing
; visualization.

(require (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 3)))
(require "../core/math.rkt")
(require "./viewer.rkt")

(provide view)

(define spheretrace-viewer%
  (class gl-viewer%
    (super-new)
    (inherit with-gl-context swap-gl-buffers low-priority-refresh)
    (inherit-field design-node)

    (init-field draw)
    (init-field [(internal-setup setup) void])

    ; Orientation of the model, as controlled by the arcball interface.
    (define orientation (quat-identity-rotation))

    ; An additional rotation that is updated during active drag gestures.  Once
    ; the drag is complete (i.e. mouse button released) this gets collapsed into
    ; 'orientation' and reset to identity.
    (define active-rotation (quat-identity-rotation))

    ; Zoom level; larger means bigger.
    (define zoom 1)

    ; Rendering "quality", which in practice is used by the spheretracer to
    ; determine epsilon when finding roots.
    (define quality 5)

    ; Rendering "step limit," used by the spheretracer to control complexity.
    (define step-limit 128)

    ; List of modes that are suitable for the current display.  'setup' must
    ; return a list of symbols that will be stashed here.
    (define modes '(#f))
    ; A tail of the 'modes' list, where the current mode is the car.
    (define remaining-modes '(#f))

    (define/override (setup)
      (set! modes (internal-setup design-node))
      (set! remaining-modes modes))

    ; Configure the GL viewport on size changes.
    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         (glViewport 0 0 width height)
         (glMatrixMode GL_PROJECTION)
         (glLoadIdentity)
         (glOrtho 0 width 0 height -10.0 10.0)
         (glMatrixMode GL_MODELVIEW)
         (glLoadIdentity)
         (glTranslated 0.0 0.0 -10.0)
         ))
      )

    ; Draw the scene.
    (define/augment (on-paint)
      (let ([width (send this get-width)]
            [height (send this get-height)])
        (draw width
              height
              (quat-mul active-rotation orientation)
              zoom
              quality
              step-limit
              (first remaining-modes))))

    ; During a mouse gesture, this gets loaded with a function that will process
    ; further events.
    (define handle-motion void)

    (define/override (on-event event)
      (let ([x (send event get-x)]
            [y (send event get-y)])
        (case (send event get-event-type)
          [(left-down)
           ; Begin a drag gesture.
           (set! handle-motion
             (let* ([w (send this get-width)]
                    [h (send this get-height)]
                    [start-v (get-arcball-vector x y w h)])
               (lambda (new-x new-y)
                 (let* ([new-v (get-arcball-vector new-x new-y w h)]
                        [rot (quat-rotation-from-to start-v new-v)])
                   (set! active-rotation (quat-rotation-from-to start-v new-v)))
                 (low-priority-refresh))))]

          [(left-up)
           ; End a drag gesture.
           (set! orientation (quat-mul active-rotation orientation))
           (set! active-rotation (quat-identity-rotation))
           (set! handle-motion void)]

          [(motion) (handle-motion x y)])))

    (define/augment (on-char event)
      (case (send event get-key-code)
        [(#\+) (set! zoom (* zoom 4/3)) (low-priority-refresh)]
        [(#\-) (set! zoom (/ zoom 4/3)) (low-priority-refresh)]
        [(#\[)
         (set! quality (max 1 (- quality 1)))
         (printf "quality now 1/~a~n" quality)
         (low-priority-refresh)]
        [(#\])
         (set! quality (+ quality 1))
         (printf "quality now 1/~a~n" quality)
         (low-priority-refresh)]
        [(#\{)
         (set! step-limit (max 1 (- step-limit 1)))
         (printf "step-limit now ~a~n" step-limit)
         (low-priority-refresh)]
        [(#\})
         (set! step-limit (+ step-limit 1))
         (printf "step-limit now ~a~n" step-limit)
         (low-priority-refresh)]
        [(#\ )
         (set! remaining-modes (rest remaining-modes))
         (when (empty? remaining-modes)
           (set! remaining-modes modes))
         (printf "mode now ~a~n" (first remaining-modes))
         (low-priority-refresh)]
        [(#\z)
         (set! orientation (quat-identity-rotation))
         (low-priority-refresh)]
        [(#\x)
         (set! orientation
           (quat-rotation-from-to (vec3 1 0 0) (vec3 0 0 1)))
         (low-priority-refresh)]
        [(#\y)
         (set! orientation
           (quat-rotation-from-to (vec3 0 1 0) (vec3 0 0 1)))
         (low-priority-refresh)]
        [(wheel-up) (set! zoom (* zoom 9/8)) (low-priority-refresh)]
        [(wheel-down) (set! zoom (/ zoom 9/8)) (low-priority-refresh)]))
  ))

; Projects the point (x, y) in a w x h view region onto a unit sphere, for use
; in producing rotation quaternions.
(define (get-arcball-vector x y w h)
  (let* ([hw (w . / . 2)]
         [hh (h . / . 2)]
         [dim (min hw hh)]
         [p (vec3 ((x . - . hw) . / . dim)
                  (- ((y . - . hh) . / . dim))
                  0)]
         [op-sqr (+ (sqr (vec3-x p)) (sqr (vec3-y p)))])
    (if (<= op-sqr 1)
      (struct-copy vec3 p [z (sqrt (1 . - . op-sqr))])
      (vec3-normalize p))))

(define (view design-path draw (setup void))
  (define frame 
    (new frame% 
         [label "Ruckus 3D"]
         [width 300]
         [height 300]))

  (define c
    (new spheretrace-viewer%
         [style '(gl no-autoclear)] 
         [parent frame] 
         [draw draw]
         [setup setup]
         [design-path design-path]))

  (send frame show #t))
