#lang racket/gui

; Extension of the basic GL viewer with features to support 3D spheretracing
; visualization.

(require racket/runtime-path)
(require (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 3)))
(require ffi/vector)

(require "../core/model.rkt")
(require "../core/math.rkt")
(require "./viewer.rkt")
(require "./glsl.rkt")

(provide spheretrace-viewer%)

; Path anchor for finding 'preamble.glsl'.  This forces a correct path to be
; ; generated at compile time, and includes the file in any generated packages.
(define-runtime-path preamble-glsl "./preamble.glsl")

; ------------------------------------------------------------------------------
; OpenGL utilities.

; Utility for getting integer shader parameters by name.
(define (get-shader-parameter shader pname)
  (let ([v (s32vector 0)])
    (glGetShaderiv shader pname v)
    (s32vector-ref v 0)))

; Utility for loading the compiler output.
(define (get-shader-info-log shader)
  (let ([log-length (get-shader-parameter shader GL_INFO_LOG_LENGTH)])
    (let-values ([(actual-length info-log)
                  (glGetShaderInfoLog shader log-length)])
      (bytes->string/utf-8 info-log #\? 0 actual-length))))

; Utilities for locating and setting uniforms.
(define (set-uniform-1i program name value)
  (glUniform1i (glGetUniformLocation program name) value))

(define (set-uniform-bool program name value)
  (set-uniform-1i program name (if value 1 0)))

(define (set-uniform-1f program name value)
  (glUniform1f (glGetUniformLocation program name)
               (real->double-flonum value)))

(define (set-uniform-2f program name value1 value2)
  (glUniform2f (glGetUniformLocation program name)
               (real->double-flonum value1)
               (real->double-flonum value2)))

(define (set-uniform-4f program name value1 value2 value3 value4)
  (glUniform4f (glGetUniformLocation program name)
               (real->double-flonum value1)
               (real->double-flonum value2)
               (real->double-flonum value3)
               (real->double-flonum value4)))

(define spheretrace-viewer%
  (class gl-viewer%
    (super-new)
    (inherit with-gl-context swap-gl-buffers low-priority-refresh)
    (inherit-field design-node)

    ; The OpenGL handle of the current shader program.  We need to keep this
    ; around so we can activate the program during rendering.  This gets set
    ; during setup.
    (field [current-program #f])

    ; The OpenGL handle of the fragment shader attached to the current program.
    ; We need to keep this around so we can explicitly delete it during
    ; recompilation.  This gets set during setup.
    (field [current-shader #f])

    ; The OpenGL handle to the texture used to communicate node colors to the
    ; shader.  This gets set during setup.
    (field [colors-texture #f])

    ; A Racket byte-string the same size as the colors texture.  We fill this
    ; in with a new copy of the texture data and then upload it using
    ; 'glTexImage2D'.  We don't strictly *need* to keep it around, but it
    ; reduces large allocations to do so.
    (field [colors-buffer #f])

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

    ; List of modes that are suitable for the current display.
    (define modes '(shaded complexity distance))
    ; A tail of the 'modes' list, where the current mode is the car.
    (define remaining-modes modes)

    (define/override (setup)
      ; Lazily allocate the byte-string used to upload colors.
      (unless colors-buffer
        (set! colors-buffer (make-bytes (* 512 512 3))))
      ; Lazily allocate the colors texture.
      (unless colors-texture
        (set! colors-texture (u32vector-ref (glGenTextures 1) 0)))

      (if design-node
        (call-with-input-file preamble-glsl generate-compile-and-link)
        ; Otherwise, at least clear the window.
        (glClear GL_COLOR_BUFFER_BIT)))

    (define (generate-compile-and-link preamble-port)
      ; Clean up leftovers from last pass.
      (when current-program
        (printf "Marking program ~a for deletion.~n" current-program)
        (glDeleteProgram current-program))
      (when current-shader
        (printf "Marking shader ~a for deletion.~n" current-shader)
        (glDeleteShader current-shader))

      (let*-values ([(source lengths)
                     (combine-sources preamble-port design-node)]
                    [(program) (glCreateProgram)]
                    [(shader) (glCreateShader GL_FRAGMENT_SHADER)])
        (glShaderSource shader (vector-length source) source lengths)
        (glCompileShader shader)

        (if (= (get-shader-parameter shader GL_COMPILE_STATUS) GL_TRUE)
          ; Success!  Finish the install.
          (begin
            (glAttachShader program shader)
            (glLinkProgram program)
            (printf "Shader program ~a compiled and linked.~n" program)
            (update-colors-texture design-node)
            (set! current-program program)
            (set! current-shader shader))
          ; Otherwise, print out the shader source for reference, along with
          ; any error message from the graphics stack.
          (begin
            (for ([line source]) (display line))
            (glDeleteShader shader)
            (glDeleteProgram program)
            (error 'generate-compile-and-link
                   "error compiling: ~a"
                   (get-shader-info-log shader))))))

    (define (combine-sources port node)
      (let* ([preamble (for/vector ([line (in-lines port)])
                        (string-append line "\n"))]
             [gen (for/vector ([line (in-list (generate-glsl node))])
                    (string-append line "\n"))]
             [lines (vector-append preamble gen)]
             [lengths (for/list ([line (in-vector lines)])
                        (string-length line))]
             [lengths-vector (list->s32vector lengths)])
        (values lines lengths-vector)))

    (define (generate-glsl node)
      (append
        (node->glsl-distance node)
        (node->glsl-disc node)))

    (define (update-colors-texture node)
      (for ([i (in-range (* 512 512 3))])
        (bytes-set! colors-buffer i #xAA))
      (printf "Collecting node colors for texture...~n")
      (collect-node-colors
        node
        (lambda (nid color)
          (bytes-set! colors-buffer
                      (* 3 nid)
                      (exact-floor ((first color) . * . 255)))
          (bytes-set! colors-buffer
                      (+ 1 (* 3 nid))
                      (exact-floor ((second color) . * . 255)))
          (bytes-set! colors-buffer
                      (+ 2 (* 3 nid))
                      (exact-floor ((third color) . * . 255)))))
      (glActiveTexture GL_TEXTURE0)
      (glBindTexture GL_TEXTURE_2D colors-texture)
      ; Yes, these parameters really need to be set, despite only using
      ; texelFetch.
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
      (glTexImage2D GL_TEXTURE_2D 0
                    GL_RGB
                    512 512 0 GL_RGB GL_UNSIGNED_BYTE
                    colors-buffer)
      (printf "Texture updated.~n"))

    ; Simple traversal routine that calls 'out-fn' with each 'node-id' and
    ; 'node-color' for any child of 'node' that has them defined.
    (define (collect-node-colors node out-fn)
      (when ((node-id node) . and . (node-color node))
        (out-fn (node-id node) (node-color node)))

      (for ([child (node-children node)])
        (collect-node-colors child out-fn)))

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
            [height (send this get-height)]
            [mode (first remaining-modes)]
            [program current-program])

        ; We'll fill the screen with a quad, used simply to hang our fragment
        ; shader upon.  These are the corners.
        (define vertex-array
          (f64vector 0.0 0.0
                     width 0.0
                     width height
                     0.0 height))

        (when program
          (glUseProgram program)

          ; Associate colors-texture to GL texture unit 0 and associate the
          ; shader's 'nodeColors' sampler with the same unit.
          (glActiveTexture GL_TEXTURE0)
          (glBindTexture GL_TEXTURE_2D colors-texture)
          (set-uniform-1i program "nodeColors" 0)

          ; Pass view parameters and rendering config into shader.
          (set-uniform-1f   program "zoom"           zoom)
          (set-uniform-1f   program "closeEnough"    (1 . / . quality))
          (set-uniform-1i   program "stepLimit"      step-limit)
          (set-uniform-bool program "showComplexity" (eq? mode 'complexity))
          (set-uniform-bool program "showDistance"   (eq? mode 'distance))

          (set-uniform-2f program "resolution" width height)

          ; Pass the view rotation quaternion to the shader, inverting its
          ; rotation since the shader has to rotate rays to match the eye
          ; instead of vice versa.
          (let ([q (quat-conjugate
                     (quat-mul active-rotation orientation))])
            (set-uniform-4f program "orientation"
                            (vec3-x (quat-v q))
                            (vec3-y (quat-v q))
                            (vec3-z (quat-v q))
                            (quat-s q))))

        ; Draw a quad.  This sequence is taken nearly verbatim from the
        ; RacketGL example code.

        ; Notify OpenGL of the existence of our vertex array.
        (let-values ([(type cptr) (gl-vector->type/cpointer vertex-array)])
          (glVertexPointer 2 type 0 cptr))
        ; Enable its use.
        (glEnableClientState GL_VERTEX_ARRAY)
        ; Draw it once.
        (glDrawArrays GL_QUADS 0 4)

        ; Clean up.
        (glDisableClientState GL_VERTEX_ARRAY)
        (when program (glUseProgram 0))))

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
