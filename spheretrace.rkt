#lang racket/gui

(require racket/flonum)
(require (planet "rgl.rkt" ("stephanh" "RacketGL.plt" 1 4)))
(require ffi/vector)
(require "viewer.rkt")
(require "edsl.rkt")
(require "compiler.rkt")
(require "math.rkt")

(provide spheretrace)

(define distance-field-source #f)

(define (generate-shader-text gen)
  (set! distance-field-source (node->glsl (call-with-edsl-root gen))))

(define (get-shader-parameter shader pname)
  (let ([v (s32vector 0)])
    (glGetShaderiv shader pname v)
    (s32vector-ref v 0)))

(define (load-program-source shader port)
  (let* ([preamble (for/vector ([line (in-lines port)])
                     (string-append line "\n"))]
         [gen (for/vector ([line (in-list distance-field-source)])
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
  (let* ([program (glCreateProgram)]
         [shader (glCreateShader GL_FRAGMENT_SHADER)]
         [lines (load-program-source shader port)])
    (glCompileShader shader)
    (unless (= (get-shader-parameter shader GL_COMPILE_STATUS) GL_TRUE)
      (for ([line lines]) (writeln line))
      (error 'load-program "error compiling: ~a" (get-shader-info-log shader)))
    (glAttachShader program shader)
    (glLinkProgram program)
    program))

(define program #f)

(define (setup)
  (if (or (gl-version-at-least? '(2 0))
          (gl-has-extension? 'GL_ARB_shader_objects))
    (set! program (call-with-input-file "preamble.glsl" load-program))
    (printf "This OpenGL does not support shaders, you'll get a plain white rectangle.~%"))
  '(shaded complexity distance))

(define (draw width height orientation quality step-limit mode)
  ; the coordinates
  (define vertex-array
    (f64vector 0.0 0.0
               width 0.0
               width height
               0.0 height))

  (define texcoord-array
    (f64vector 0 0
               0.5 0
               0.5 0.5
               0 0.5))


  (when program
    (glUseProgram program)

    (let ([ceU (glGetUniformLocation program "closeEnough")])
      (glUniform1f ceU (real->double-flonum (/ 1 quality))))

    (let ([slU (glGetUniformLocation program "stepLimit")])
      (glUniform1i slU step-limit))

    (let ([scU (glGetUniformLocation program "showComplexity")])
      (glUniform1i scU (if (eq? mode 'complexity) 1 0)))

    (let ([scU (glGetUniformLocation program "showDistance")])
      (glUniform1i scU (if (eq? mode 'distance) 1 0)))

    (let ([resU (glGetUniformLocation program "resolution")])
      (glUniform2f resU (->fl width) (->fl height))))

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
  (let-values (((type cptr) (gl-vector->type/cpointer texcoord-array)))
    (glTexCoordPointer 2 type 0 cptr))
  ; 2. Yes, I really want you to use it, I was not simply fooling around.
  (glEnableClientState GL_VERTEX_ARRAY)
  (glEnableClientState GL_TEXTURE_COORD_ARRAY)
  ; 3. Allright, now draw the silly thing already!
  (glDrawArrays GL_QUADS 0 4)

  ; Clean up state.
  (glDisableClientState GL_TEXTURE_COORD_ARRAY)
  (glDisableClientState GL_VERTEX_ARRAY)
  (when program
    (glUseProgram 0)))

(define (spheretrace gen)
  (generate-shader-text gen)
  (view draw setup))
