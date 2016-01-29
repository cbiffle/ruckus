#lang racket

; #lang support.

(provide
  ; Hide Racket's module-begin form from Ruckus.
  (except-out (all-from-out racket/base)
              #%module-begin)
  ; Substitute our own.
  (rename-out [ruckus:module-begin #%module-begin]))

; Redundant import, but required by all-from-out above.
(require racket/base)

; Reshapes modules to have some implicit features, to reduce boilerplate in
; design files:
; - Automatically requires the EDSL module.
; - Wraps all top-level content in an implicit 'design' definition.
; - Provides 'design' for access by the system.
(define-syntax (ruckus:module-begin stx)
  (syntax-case stx ()
    [(_ expr ...)
     ; Promote a couple of symbols to real-live identifiers in the loaded
     ; module's lexical context.
     (with-syntax ([design$ (datum->syntax stx 'design)]
                   [ruckus$ (datum->syntax stx 'ruckus)])
       #`(#%module-begin
          (provide design$)
          (require ruckus$)
          (define (design$) expr ... (void))))]))

