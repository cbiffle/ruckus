#lang racket

; Basic design file loading support.

(provide load-frep)

(require racket/rerequire)

; Loads the Racket module at 'path', reloading it if it has changed.  Returns
; the module's binding for 'design'.
(define (load-frep path)
  (dynamic-rerequire path)
  (dynamic-require path 'design))
