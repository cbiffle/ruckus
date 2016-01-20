#lang racket

(require "../core/compiler/lower.rkt")
(require "./evaluator.rkt")
(require "./loader.rkt")

(define (irdump path)
  (let*-values ([(gen) (load-frep path)]
                [(d i ss) (generate-statements (call-with-edsl-root gen))])
    (for ([s (in-list ss)])
      (pretty-write s))
    (printf "Distance in: ~a~n" d)
    (printf "ID in:       ~a~n" i)))

(command-line
  #:program "irdump"
  #:args (path)
  (irdump path))
