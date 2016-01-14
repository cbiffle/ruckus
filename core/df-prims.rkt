#lang racket

(require "./math.rkt")

(provide
  df-sphere
  df-box)

(define (df-sphere r q)
  (- (vec3-length q) r))

(define (df-box corner q)
  (let ([d (vec3-sub (vec3-abs q) corner)])
    (+ (min (max (vec3-x d) (vec3-y d) (vec3-z d)) 0)
       (vec3-length (vec3-max d (vec3 0 0 0))))))

