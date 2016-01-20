#lang racket

(require "../core/compiler/lower.rkt")
(require "./evaluator.rkt")
(require "./loader.rkt")

(define prune-discriminator #f)

(define (irdump path)
  (let*-values ([(gen) (load-frep path)]
                [(d i ss) (generate-statements (call-with-edsl-root gen))])
    (if prune-discriminator
      (begin
        (for ([s (in-list (prune-statements ss d))])
          (pretty-write s))
        (printf "Distance in: ~a~n" d))
      (begin
        (for ([s (in-list ss)])
          (pretty-write s))
        (printf "Distance in: ~a~n" d)
        (printf "ID in:       ~a~n" i)))))

(command-line
  #:program "irdump"
  #:once-each
  [("-p" "--prune")
   "Prune node discrimination code."
   (set! prune-discriminator #t)]
  #:args (path)
  (irdump path))
