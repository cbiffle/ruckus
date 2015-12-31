;; A simple viewer window for OpenGL.
;; Allows user to rotate and zoom the scene.
#lang racket/gui

(require (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 3)))
(require "math.rkt")

(provide view)

(define (get-arcball-vector x y w h)
  (let* ([dim (min w h)]
         [p (vec3 (((x . / . dim) . * . 2) . - . 1)
                  (- (((y . / . dim) . * . 2) . - . 1))
                  0)]
         [op-sqr (+ (sqr (vec3-x p)) (sqr (vec3-y p)))])
    (if (<= op-sqr 1)
      (struct-copy vec3 p [z (sqrt (1 . - . op-sqr))])
      (vec3-normalize p))))



(define gl-viewer%
  (class canvas%
    (super-new)
    (inherit with-gl-context swap-gl-buffers refresh)

    (init-field draw)
    (init-field (setup void))

    (define setup-called #f)
    (define orientation (quat-identity-rotation))
    (define active-rotation (quat-identity-rotation))
    (define zoom 1)

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
 
    (define/override (on-paint)
      (with-gl-context               
        (lambda ()
          (unless setup-called
            (setup)
            (set! setup-called #t))
          (send this get-width)
          (glClearColor 0.0 0.0 0.3 0.0) ; darkish blue
          (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
          (glPushMatrix)
          (let ([width (send this get-width)]
                [height (send this get-height)])
            (draw width height (quat-mul orientation active-rotation)))
          (glPopMatrix)
          (swap-gl-buffers)
          ))
      )

    (define handle-motion void)

    (define/override (on-event event)
      (let ((x (send event get-x))
            (y (send event get-y)))
        (case (send event get-event-type)
          ((left-down)
           (set! handle-motion
             (let* ([w (send this get-width)]
                    [h (send this get-height)]
                    [start-v (get-arcball-vector x y w h)])
               (lambda (new-x new-y)
                 (let* ([new-v (get-arcball-vector new-x new-y w h)]
                        [rot (quat-rotation-from-to start-v new-v)])
                   (set! active-rotation (quat-rotation-from-to start-v new-v)))
                 (refresh)))))
          ((left-up)
           (set! orientation (quat-mul orientation active-rotation))
           (set! active-rotation (quat-identity-rotation))
           (set! handle-motion void))
          ((motion) (handle-motion x y)))))

    (define/override (on-char event)
      (case (send event get-key-code)
        ((#\+) (set! zoom (* zoom 4/3)) (refresh))
        ((#\-) (set! zoom (/ zoom 4/3)) (refresh))
        ((wheel-up) (set! zoom (* zoom 9/8)) (refresh))
        ((wheel-down) (set! zoom (/ zoom 9/8)) (refresh))))
  ))


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
