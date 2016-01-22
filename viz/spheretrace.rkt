#lang racket/gui

(require racket/runtime-path)

(require (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 4)))
(require ffi/vector)

(require "../core/compiler/enumerate.rkt")
(require "../core/math.rkt")
(require "../core/model.rkt")
(require "../lang/evaluator.rkt")
(require "../lang/loader.rkt")
(require "./glsl.rkt")
(require "./viewer.rkt")

(define design-path #f)
(define program #f)
(define delete-program void)
(define colors-texture #f)
(define colors-buffer #f)

(define (reload path)
  (let ([gen (load-frep path)])
    (unless (procedure? gen)
      (error "Design at" path "binds 'design', but not to a procedure."))
    (let-values ([(_ node) (enumerate-nodes 0 (call-with-edsl-root gen))])
      (update-colors-texture node)
      (append
        (node->glsl-distance node)
        (node->glsl-disc node)))))

(define (update-colors-texture node)
  (printf "Resetting texture colors...~n")
  (for ([i (in-range (* 512 512 3))])
    (bytes-set! colors-buffer i #x80))
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
   
(define (collect-node-colors node out-fn)
  (when ((node-id node) . and . (node-color node))
    (out-fn (node-id node) (node-color node)))

  (for ([child (node-children node)])
    (collect-node-colors child out-fn)))

(define (get-shader-parameter shader pname)
  (let ([v (s32vector 0)])
    (glGetShaderiv shader pname v)
    (s32vector-ref v 0)))

(define (load-program-source shader port)
  (let* ([preamble (for/vector ([line (in-lines port)])
                     (string-append line "\n"))]
         [gen (for/vector ([line (in-list (reload design-path))])
                (string-append line "\n"))]
         [lines (vector-append preamble gen)]
         (sizes (for/list ((line (in-vector lines))) (string-length line)))
         (sizes (list->s32vector sizes)))
   (glShaderSource shader (vector-length lines) lines sizes)
   lines))

(define (get-shader-info-log shader)
  (let ([log-length (get-shader-parameter shader GL_INFO_LOG_LENGTH)])
    (let-values ([(actual-length info-log)
                  (glGetShaderInfoLog shader log-length)])
      (bytes->string/utf-8 info-log #\? 0 actual-length))))

(define (load-program port)
  (when program
    (delete-program)
    (set! program #f)
    (set! delete-program void))

  (let* ([program (glCreateProgram)]
         [shader (glCreateShader GL_FRAGMENT_SHADER)]
         [lines (load-program-source shader port)])
    (glCompileShader shader)
    (unless (= (get-shader-parameter shader GL_COMPILE_STATUS) GL_TRUE)
      (for ([line lines]) (writeln line))
      (error 'load-program "error compiling: ~a" (get-shader-info-log shader)))
    (glAttachShader program shader)
    (glLinkProgram program)
    (printf "Shader program ~a compiled and linked.~n" program)
    (values
      program
      (lambda ()
        (printf "Detaching shader ~a from program ~a~n" shader program)
        (glDetachShader program shader)
        (printf "Marking shader ~a for deletion.~n" shader)
        (glDeleteShader shader)
        (printf "Marking program ~a for deletion.~n" program)
        (glDeleteProgram program)))))

(define-runtime-path preamble-glsl "./preamble.glsl")

(define (setup)
  (unless colors-buffer
    (set! colors-buffer (make-bytes (* 512 512 3))))
  (unless colors-texture
    (set! colors-texture (u32vector-ref (glGenTextures 1) 0)))

  (if (or (gl-version-at-least? '(2 0))
          (gl-has-extension? 'GL_ARB_shader_objects))
    (set!-values (program delete-program)
                 (call-with-input-file preamble-glsl load-program))
    (printf "This OpenGL does not support shaders, you'll get a plain white rectangle.~%"))
  '(shaded complexity distance))

(define (draw width height orientation zoom quality step-limit mode)
  ; the coordinates
  (define vertex-array
    (f64vector 0.0 0.0
               width 0.0
               width height
               0.0 height))

  (when program
    (glUseProgram program)

    (glActiveTexture GL_TEXTURE0)
    (glBindTexture GL_TEXTURE_2D colors-texture)
    (let ([u (glGetUniformLocation program "nodeColors")])
      (glUniform1i u 0))

    (let ([zU (glGetUniformLocation program "zoom")])
      (glUniform1f zU (real->double-flonum zoom)))

    (let ([ceU (glGetUniformLocation program "closeEnough")])
      (glUniform1f ceU (real->double-flonum (/ 1 quality))))

    (let ([slU (glGetUniformLocation program "stepLimit")])
      (glUniform1i slU step-limit))

    (let ([scU (glGetUniformLocation program "showComplexity")])
      (glUniform1i scU (if (eq? mode 'complexity) 1 0)))

    (let ([scU (glGetUniformLocation program "showDistance")])
      (glUniform1i scU (if (eq? mode 'distance) 1 0)))

    (let ([resU (glGetUniformLocation program "resolution")])
      (glUniform2f resU
                   (real->double-flonum width)
                   (real->double-flonum height))))

    (let ([orientU (glGetUniformLocation program "orientation")]
          [qv (quat-v (quat-conjugate orientation))])
      (glUniform4f orientU
                   (real->double-flonum (vec3-x qv))
                   (real->double-flonum (vec3-y qv))
                   (real->double-flonum (vec3-z qv))
                   (real->double-flonum (quat-s orientation))))

  ; Let's be "modern" and use the array functions (introduced in OpenGL 1.1).
  ; Note that you need to ask GL everything 3 times:
  ; 1. Here is an array I'd like you to draw...
  (let-values (((type cptr) (gl-vector->type/cpointer vertex-array)))
    (glVertexPointer 2 type 0 cptr))
  ; 2. Yes, I really want you to use it, I was not simply fooling around.
  (glEnableClientState GL_VERTEX_ARRAY)
  ; 3. Allright, now draw the silly thing already!
  (glDrawArrays GL_QUADS 0 4)

  ; Clean up state.
  (glDisableClientState GL_VERTEX_ARRAY)
  (when program
    (glUseProgram 0)))

(define (spheretrace path)
  (set! design-path path)
  (view draw setup))

(command-line
  #:program "spheretrace"
  #:args (path)
  (spheretrace path))
