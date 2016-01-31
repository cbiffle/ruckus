#lang racket

; The 'opengl' package takes a *really long time* to load.  There's no reason
; to load it to process command lines like "ruckus-3d --help".  This is my
; attempt at making it lazy.

(provide lazy-require)

(require racket/runtime-path)

(define-syntax-rule (lazy-require mod sym ...)
  (begin
    (define-namespace-anchor anchor)
    (define-runtime-module-path-index mpi 'mod)
    (define-syntax sym
      (syntax-id-rules ()
        [sym 
          (parameterize ([current-namespace
                           (namespace-anchor->namespace anchor)])
            (dynamic-require mpi 'sym))])) ...))

