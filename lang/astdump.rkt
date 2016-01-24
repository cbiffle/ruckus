#lang racket

(require "../core/compiler/canon.rkt")
(require "../core/compiler/enumerate.rkt")
(require "./evaluator.rkt")
(require "./loader.rkt")

(define (astdump path)
  (let-values ([(_ n) (enumerate-nodes
                        0
                        (process
                          (call-with-edsl-root (load-frep path))))])
    (pretty-write n)))

(define process (lambda (x) x))

(command-line
  #:program "astdump"

  #:usage-help
  "Dumps the EDSL-level abstract syntax tree for a design."

  #:once-each
  [("-c" "--canonical")
   "Rewrite the AST into canonical form."
   (set! process (compose first canonicalize))]

  #:args (design-path)
  (astdump design-path))
