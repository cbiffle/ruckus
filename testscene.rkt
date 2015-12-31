#lang racket

(require "edsl.rkt")
(require "spheretrace.rkt")

(define (scene)
  (for ([i 5])
    (translate (list (* i 100) 0 0)
               (difference
                 (sphere 100)
                 (translate '[0 20 0] (sphere 100))
                 )))

  (translate '(0 125 0)
             (rotate '(0 1 0) 20
                     (cube 50)))
  )


(spheretrace scene)
