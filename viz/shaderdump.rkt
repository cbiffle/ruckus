#lang racket

; Tool for dumping GLSL generated shaders.

(require "../lang/evaluator.rkt")
(require "../lang/loader.rkt")
(require "../core/compiler/canon.rkt")
(require "../core/compiler/enumerate.rkt")
(require "./glsl.rkt")

(define (apply-compiler c node)
  (let-values ([(_ node) (enumerate-nodes 0
                           (first
                             (canonicalize node)))])
    (c node)))
                             
(define compiler node->glsl-distance)

(define (shaderdump path)
  (let ([gen (load-frep path)])
    (for ([line (in-list (apply-compiler compiler (call-with-edsl-root gen)))])
      (displayln line))))

(define (run)
  (command-line
    #:program "ruckus-dump-glsl"

    #:usage-help
    "Prints the GLSL code equivalent of a design.  Mostly useful for debugging."

    #:once-any
    [("-d" "--distance")
     "Generate shader in distance field evaluator mode (default)."
     (set! compiler node->glsl-distance)]
    [("-i" "--id")
     "Generate shader in ID discrimination mode."
     (set! compiler node->glsl-disc)]

    #:args (design-path)
    (shaderdump design-path)))

(module+ main (run))
