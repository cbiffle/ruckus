#lang racket

(provide
  load-frep)

(require syntax/modread)
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
      (namespace-require "edsl.rkt")
      (namespace-require "math.rkt")
      (call-with-input-file path (lambda (f)
                                   (port-count-lines! f)
                                   (read-and-eval-syntax f ns)))
      (namespace-variable-value
        'design
        #f  ; don't interpret as reference, just retrieve binding
        #f  ; no failure thunk, just throw.
        ns))))

(define (read-and-eval-syntax port ns)
  (let ([x (read-syntax (object-name port) port)])
    (unless (eof-object? x)
      (eval-syntax (namespace-syntax-introduce x) ns)
      (read-and-eval-syntax port ns))))
