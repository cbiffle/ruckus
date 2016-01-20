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
  #:once-each
  [("-m" "--mode")
   mode-str
   "Select mode for generated code: distance (default) or id."
   (case mode-str
     [("distance") (set! make-s-expr node->distance-s-expr)]
     [("id") (set! make-s-expr node->disc-s-expr)]
     [else (error "Bad mode value:" mode-str)])]
  #:args (path)
  (rktdump path))
