#lang racket

(require "../lang/evaluator.rkt")
(require "../lang/loader.rkt")
(require "./glsl.rkt")

(define compiler node->glsl-distance)

(define (shaderdump path)
  (let ([gen (load-frep path)])
    (for ([line (in-list (compiler (call-with-edsl-root gen)))])
      (displayln line))))

(command-line
  #:program "shaderdump"

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
  (shaderdump design-path))
