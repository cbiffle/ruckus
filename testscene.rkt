#lang racket

(require "edsl.rkt")
(require "spheretrace.rkt")

(define (scene)
  (for ([i 5])
    (translate (list (* i 100) 0 0) (sphere 100))))

(spheretrace scene)
