#lang racket

(require "../core/compiler/racket.rkt")
(require "./evaluator.rkt")
(require "./loader.rkt")

(define make-s-expr node->distance-s-expr)

(define (rktdump path)
  (let* ([gen (load-frep path)]
         [s (make-s-expr (call-with-edsl-root gen))])
    (pretty-write s)))

(command-line
  #:program "rktdump"

  #:usage-help
  "Dumps the Racket expression describing a design mathematically."

  #:once-any
  [("-d" "--distance")
   "Generate distance field evaluator code (default)."
   (set! make-s-expr node->distance-s-expr)]
  [("-i" "--id")
   "Generate node ID discriminator code."
     (set! make-s-expr node->disc-s-expr)]

  #:args (design-path)
  (rktdump design-path))
