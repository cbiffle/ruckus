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
  #:once-each
  [("-m" "--mode")
   mode-str
   "Select shader mode: distance or id"
   (case mode-str
     [("distance") (set! compiler node->glsl-distance)]
     [("id") (set! compiler node->glsl-disc)]
     [else (error "bad mode value:" mode-str)])]
  #:args (path)
  (shaderdump path))
