#lang racket

; #lang support.
;
; This is a language file intended for use with syntax/module-reader.  It gets
; loaded by ruckus/lang/reader.
;
; It can be a bit hard to see the forest for the trees in the code below, so
; here's a high-level summary.
;
; We want to lower the Ruckus design language into the Racket language.
; Ruckus's design language has these key differences from Racket:
;
; 1. The ruckus module is implicitly required.
;
; 2. Ruckus CSG forms can appear at the file's top level.  They are collected
;    into a design evaluation function instead of executing directly.
;
; After lowering, nearly everything at the top level of a design language
; module gets wrapped into the body of a function (called, internally,
; 'design').  Exceptions are made for Racket forms that, syntactically, cannot
; occur inside a function, like 'require' and 'provide'.  See below for a
; precise list.
;
; In addition, a design language module contains a submodule called
; 'ruckus-metadata'.  This is where Ruckus's symbolic names are provided, so
; that they don't conflict with names used in the design (or in other design
; language modules required by the design).  Currently, the following names are
; provided by the metadata submodule:
;
; - top-level-thunk: the design evaluator thunk, which can be used with
;   call-with-edsl-root to produce a design AST.

(provide
  ; Hide Racket's module-begin form from Ruckus.
  (except-out (all-from-out racket/base)
              #%module-begin)
  ; Substitute our own.
  (rename-out [ruckus:module-begin #%module-begin]))

; Redundant import, but required by all-from-out above.
(require racket/base)

(require (for-syntax syntax/kerncase))

(begin-for-syntax
  ; Forms that will not be expanded during macro processing by convert-body,
  ; below.  This rewriter has the opportunity to act specially on these forms
  ; before they are expanded.
  (define expand-stop-ids
    (append (kernel-form-identifier-list)
            ; Additionally block expansion of require, since we'd like to
            ; handle it before it gets lowered.
            (syntax->list #'(provide require))))
 
  ; Identifiers of forms that will be hoisted out of the evaluator thunk and
  ; into the module's top level.
  ;
  ; Note that any macro identifier included here must also appear in
  ; expand-stop-ids, or it will expand before we can inspect it for hoisting.
  (define hoisted-ids
    (syntax->list #'(require
                     provide
                     define-values
                     define-syntaxes
                     begin-for-syntax
                     module
                     module*
                     #%require
                     #%provide
                     #%declare))))


; Rewrites a Ruckus syntax form into a Racket module using the general rules
; described above.
(define-syntax (ruckus:module-begin stx)
  (syntax-case stx ()
    [(_ expr ...)
     ; Promote a couple of symbols to real-live identifiers in the loaded
     ; module's lexical context.
     (with-syntax ([design$ (datum->syntax stx (gensym "design"))]
                   [ruckus$ (datum->syntax stx 'ruckus)]
                   [parts$ (datum->syntax stx null)])
       #`(#%module-begin
          (require ruckus$)

          (convert-body design$ parts$ expr ...)

          (module* ruckus-metadata #f
            (provide top-level-thunk)
            (define top-level-thunk design$)
            )))]))

; Recursive macro helper for rewriting Ruckus modules into Racket modules.
;
; The goal here is to lift certain language forms to the top level, where
; they're required to live, while packaging everything else into a single
; definition.
;
; This is patterned after the approach used by Scribble (scribble/doclang),
; with some simplifications.
(define-syntax (convert-body stx)
  (syntax-case stx ()
    ; Termination case: when we have no more exprs, unroll the list of forms
    ; that we've accumulated for the design definition.  Suppress any return
    ; value from the definition for now, since it may not be obvious which form
    ; is last.
    [(_ design-id parts)
     #`(define (design-id)
         (begin . #,(reverse (syntax->list #'parts)))
         (void))]

    ; While we have at least one expr in the body, we must expand it and
    ; examine it to figure out where it goes.
    [(_ design-id parts e . exprs)
     ; Perform some basic macro expansion but leave kernel macros and the
     ; user-level require/provide syntax untouched.  They'll expand later.
     (let ([expanded (local-expand #'e 'module expand-stop-ids)])
       (syntax-case expanded (begin)
         ; Flatten top-level begin forms.  This exposes to this analysis any
         ; recognized forms within a top-level begin.  This is important because
         ; macros often produce begins containing things like require.
         [(begin body ...)
          #`(convert-body design-id parts body ... . exprs)]
         ; Recognize other top-level forms from the canned list below.  Expand
         ; them directly into our output instead of postponing them to the
         ; design definition.
         [(form-id . _)
          (and (identifier? #'form-id)
               (ormap (lambda (kw) (free-identifier=? #'form-id kw))
                      hoisted-ids))
          #`(begin #,expanded (convert-body design-id parts . exprs))]
         ; Other forms just get postponed.
         [_else
           (with-syntax ([parts (cons #'e #'parts)])
             #`(convert-body design-id parts . exprs))]))]))
