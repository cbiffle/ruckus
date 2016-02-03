#lang ruckus

(require racket/math)
(require ruckus/core/math)

; Produces an inexact real in the range +/- 'range'.
(define (random+/- range)
  (- (* (random) range 2) range))

; ------------------------------------------------------------------------------
; Holographic surface approximation.
;
; The idea: given a distance field of arbitrary complexity, sample the field on
; the surface of a bounding sphere, and then fit a surface to those samples as
; constraints.
;
; This produces an interesting (but not obviously useful) surface
; simplification.

; Produces a random vec3 on the surface of a sphere with the given radius.
(define (random-point-on-sphere radius)
  (let* ([x1 (random+/- 1)]
         [x2 (random+/- 1)])
    (if (((sqr x1) . + . (sqr x2)) . >= . 1)
      (random-point-on-sphere radius)
      (vec3-mul radius
                (vec3 (* 2 x1 (sqrt (- 1 (sqr x1) (sqr x2))))
                      (* 2 x2 (sqrt (- 1 (sqr x1) (sqr x2))))
                      (- 1 (* 2 (+ (sqr x1) (sqr x2)))))))))

(define (spherical-hologram n-samples radius df #:inside [inside (vec3 0 0 0)])
  (unless (negative? (df inside))
    (error "That 'inside' point you specified isn't inside anything:"
           inside))
  (interpolation-surface-raw
    (cons
      (cons inside (df inside))
      (for/list ([i n-samples])
        (let ([v (random-point-on-sphere radius)])
          (cons v (df v)))))))

; ------------------------------------------------------------------------------
; Monte Carlo surface approximation.
;
; Similar to the holographic approximation, but the sample points are randomly
; distributed within an axis-aligned cube.  This can (unpredictably) recover
; more local detail, but the results are subjectively uglier.

; Demo geometry.
(define (blend-test)
  (smooth-union 110
    (radial-repeat 30
      (smooth-union 130
        (color 'lime (sphere 200))
        (at '[310 0 0]
          (color 'red (sphere 50)))))
    (at '[0 0 400] (color 'blanched-almond (sphere 150)))
    (at '[0 0 -300] (cube 100))
    (at '[0 0 -400] (color 'purple (sphere 50)))))

; Generates an interpolation surface using randomly generated constraints
; derived from the distance field evaluator 'df'.
;
; 'n-samples' constraints will be generated within a unit cube of +/- 'range'
; units around the origin.
(define (monte-carlo-sample n-samples range df)
  (interpolation-surface-raw
    (for/list ([i n-samples])
      (let ([p (vec3 (random+/- range)
                     (random+/- range)
                     (random+/- range))])
        (cons p (df p))))))


(spherical-hologram 700 500
  (reflect-distance
    (blend-test)))
