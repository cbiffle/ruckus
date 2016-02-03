#lang racket

; Basic design file loading support.

(provide load-frep)

(require racket/rerequire)

; Loads the Racket module at 'path', reloading it if it has changed.  Returns
; the module's binding for 'design'.
(define (load-frep path)
  ; Racket won't permit modules to be loaded using absolute paths.  Instead, we
  ; make a valid module path by generating the (potentially ridiculous)
  ; relative path.
  (let* ([abs-path (simplify-path (path->complete-path path))]
         [rel-path (find-relative-path (current-directory) abs-path)])
    (dynamic-rerequire rel-path)
    (dynamic-require `(submod ,rel-path ruckus-metadata) 'top-level-thunk)))
