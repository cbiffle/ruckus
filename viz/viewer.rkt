#lang racket/gui

; Basic GL viewer with arcball support.  Based heavily on the RacketGL example
; code.

(require (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 3)))
(require "../core/math.rkt")

(provide view)

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

    ; Flag used to coalesce queued low-priority refresh events.
    (define refresh-queued #f)

    ; Triggers a low-priority refresh event.  Display refresh events in Racket
    ; are normally higher priority than input events, which ensures that we'll
    ; get one refresh per e.g. mouse movement.  This is really bad if refresh is
    ; expensive.  This low-priority refresh mechanism lets us easily coalesce
    ; many input events into a single refresh.
    (define (low-priority-refresh)
      (unless refresh-queued
        (queue-callback (lambda ()
                          (send this refresh)
                          (set! refresh-queued #f))
                        #f)  ; <-- makes it low priority
        (set! refresh-queued #t)))

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
    (define/override (on-paint)
      (with-gl-context               
        (lambda ()
          ; Lazily call setup if required.
          (unless setup-called
            (set! modes (setup))
            (set! remaining-modes modes)
            (set! setup-called #t))
          (glPushMatrix)
          (let ([width (send this get-width)]
                [height (send this get-height)])
            (draw width
                  height
                  (quat-mul active-rotation orientation)
                  zoom
                  quality
                  step-limit
                  (first remaining-modes)))
          (glPopMatrix)
          (swap-gl-buffers))))

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

    (define/override (on-char event)
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
        [(f5) (set! setup-called #f) (low-priority-refresh)]
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

(define (show-gl-info frame canvas)
  (let-values (((renderer version vendor)
                (send canvas with-gl-context
                      (lambda () 
                        (values
                          (glGetString GL_RENDERER)
                          (glGetString GL_VERSION)
                          (glGetString GL_VENDOR))))))
    (define label
      (format "RENDERER: ~a~%VERSION: ~a~%VENDOR: ~a"
              renderer version vendor))
    (define dialog (new dialog% [parent frame] [label "OpenGL info"]))          
    (define msg (new message%
                     [parent dialog]
                     [label label]))
    (define extensions-list (new list-box% 
                                 [parent dialog] 
                                 [label "EXTENSIONS:"]
                                 [style '(single vertical-label)]
                                 [choices
                                   (sort
                                     (for/list ((ext (in-set (gl-extensions))))
                                       (symbol->string ext))
                                     string<?)]))
    (send dialog show #t)))


(define (view draw (setup void))
  (define frame 
    (new frame% 
         [label "OpenGL viewer"]
         [width 300]
         [height 300]))

  (define menubar
    (new menu-bar% [parent frame]))

  (define help-menu
    (new menu% [parent menubar] [label "&Help"]))

  (define c
    (new gl-viewer%
         (style '(gl no-autoclear)) 
         (parent frame) 
         (draw draw)
         (setup setup)))

  (define gl-info-item
    (new menu-item% [parent help-menu] [label "GL info"]
         [callback (lambda (i e) (show-gl-info frame c))]))

  (send frame show #t))
