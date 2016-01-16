#lang racket

(require "../core/compiler.rkt")
(require "./evaluator.rkt")
(require "./loader.rkt")

(define (astdump path)
  (let-values ([(_ n) (enumerate-nodes
                        0
                        (call-with-edsl-root (load-frep path)))])
    (pretty-write n)))

(command-line
  #:program "astdump"
  #:args (path)
  (astdump path))
