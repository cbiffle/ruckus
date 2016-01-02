#lang racket

(require "edsl.rkt")
(require "spheretrace.rkt")

(define (scene)
  (repeat-x 200 (bowl 100 20))

  (translate '(0 125 0)
             (rotate '(0 1 0) 20
                     (cube 50)))
  (translate '[0 0 -150] (extrude 20 (flat-part)))
  )

(define (flat-part)
  (mirror-x
    (mirror-y
      (difference
        (rect 300 400)
        (translate '[100 200 0]
                   (circle 25))
        ))))

(define (half-sphere radius)
  (intersection
    (sphere radius)
    (half-space '[0 1 0] 0)))

(define (bowl radius thickness)
  (difference
    (half-sphere radius)
    (translate `[0 ,thickness 0] (half-sphere radius))))

(spheretrace scene)
