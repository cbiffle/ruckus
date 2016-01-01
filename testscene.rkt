#lang racket

(require "edsl.rkt")
(require "spheretrace.rkt")

(define (scene)
  (for ([i 5])
    (translate (list (* i 100) 0 0)
               (bowl 100 20)))

  (translate '(0 125 0)
             (rotate '(0 1 0) 20
                     (cube 50)))
  )

(define (bowl radius thickness)
  (difference
    (intersection
      (sphere radius)
      (half-space '[0 1 0] 0))
    (translate `[0 ,thickness 0] (sphere radius))))

(spheretrace scene)
