#lang racket

; Stuff needed to evaluate distance functions in generated Racket code.

(provide
  radial-project
  smooth-min
  df-sphere
  df-box
  df-capsule
  df-rect
  df-circle)

(require "./math.rkt")

(define (radial-project q period shift)
  (let* ([a (if (zero? (vec3-x q))
              (* (sgn (vec3-y q)) (/ pi 2))
              (atan (vec3-y q) (vec3-x q)))]
         [d (sqrt (+ (sqr (vec3-x q)) (sqr (vec3-y q))))]
         [ap (+ (real-mod (+ a (period . / . 2)) period)
                (- (period . / . 2))
                shift)])
    (vec3 (* d (cos ap))
          (* d (sin ap))
          (vec3-z q))))

(define (smooth-min k a b)
  (let ([h (clamp (+ 0.5 (* 0.5 (/ (- b a) k))) 0. 1.)])
    (- (mix b a h)
       (* k h (- 1. h)))))

(define (mix x y a)
  (+ (* x (- 1 a))
     (* y a)))

(define (clamp x min-val max-val)
  (min (max x min-val) max-val))

(define (df-sphere r q)
  (- (vec3-length q) r))

(define (df-box corner q)
  (let ([d (vec3-sub (vec3-abs q) corner)])
    (+ (min (max (vec3-x d) (vec3-y d) (vec3-z d)) 0)
       (vec3-length (vec3-max d (vec3 0 0 0))))))

(define (df-capsule h r q)
  (let* ([q (vec3-abs q)]
         [t (clamp (((vec3-z q) . * . h) . / . (h . * . h)) 0 1)])
    (- (vec3-length (vec3-sub q (vec3 0 0 (h . * . t))))
       r)))

(define (df-rect w h q)
  ; TODO: bit of a hack since I have no first-class vec2
  (let* ([q (vec3 (vec3-x q) (vec3-y q) 0)]
         [d (vec3-sub (vec3-abs q) (vec3 w h 0))])
    (+ (min (max (vec3-x d) (vec3-y d)) 0)
       (vec3-length (vec3-max d (vec3 0 0 0))))))

(define (df-circle r q)
  ; TODO: bit of a hack since I have no first-class vec2
  (- (vec3-length (vec3 (vec3-x q) (vec3-y q) 0)) r))


