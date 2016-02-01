#lang racket

; A tool for dumping internal compiler IR.

(require "../core/compiler/lower.rkt")
(require "./evaluator.rkt")
(require "./loader.rkt")

(define pruner (lambda (ss d) ss))
(define pruning #f)

(define (irdump path)
  (let*-values ([(gen) (load-frep path)]
                [(d i ss) (generate-statements (call-with-edsl-root gen))])
    (for ([s (in-list (pruner ss d))])
      (pretty-write s))
    (printf "Distance in: ~a~n" d)
    (unless pruning (printf "ID in:       ~a~n" i))))

(define (run)
  (command-line
    #:program "ruckus-dump-ir"

    #:usage-help
    "Dumps the internal compiler IR for a design."

    #:once-each
    [("-p" "--prune")
     "Prune node discrimination code."
     (begin
       (set! pruning #t)
       (set! pruner prune-statements))]

    #:args (design-path)
    (irdump design-path)))

(module+ main (run))
