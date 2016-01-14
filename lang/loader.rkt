#lang racket

(provide
  load-frep)

(require racket/runtime-path)
(require syntax/modread)

(require "../core/math.rkt")
(require "./edsl.rkt")

; Get pointers to the modules that we bind into the EDSL environment, and
; put 'em all in a list.
(define-runtime-module-path-index mpi-math "../core/math.rkt")
(define-runtime-module-path-index mpi-edsl "./edsl.rkt")
(define modules (list mpi-math mpi-edsl))

; Loads the Racket module at 'path', reloading it if it has changed.  Returns
; the module's binding for 'design'.
(define (load-frep path)
  (let ([ns (make-base-namespace)])
    ; Attach a shared copy of the relevant modules from the current namespace,
    ; before entering the new one.
    (for ([m (in-list modules)])
      (namespace-attach-module (current-namespace)
                               (module-path-index-resolve m)
                               ns))

    (parameterize ([current-namespace ns])
      ; Require the modules so that designs don't have to.
      (for ([m (in-list modules)])
        (namespace-require (module-path-index-resolve m)))

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
