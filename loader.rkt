#lang racket

(provide
  load-frep)

(require racket/rerequire)
(require "edsl.rkt")
(require "math.rkt")

; Loads the Racket module at 'path', reloading it if it has changed.  Returns
; the module's binding for 'design'.
(define (load-frep path)
  (let ([ns (make-base-namespace)])
    (namespace-attach-module (current-namespace)
                             "edsl.rkt"
                             ns)
    (namespace-attach-module (current-namespace)
                             "math.rkt"
                             ns)
    (parameterize ([current-namespace ns])
      (dynamic-rerequire path #:verbosity 'reload)
      (namespace-variable-value
        'design
        #f  ; don't interpret as reference, just retrieve binding
        #f  ; no failure thunk, just throw.
        (module->namespace path)))))
