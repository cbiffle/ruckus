#lang racket

; A simple and probably slow collection of vector/quaternion/matrix routines
; supporting my CAD project.

(provide
  (struct-out vec3)
  vec3-normalize

  (struct-out quat)
  quat-mul
  quat-normalize
  quat-rotate
  quat-rotation-from-to
  quat-rotation-around)

; ------------------------------------------------------------------------------
; 3-vectors

(struct vec3 (x y z) #:transparent)

(define/match (vec3-negate v)
  [((vec3 x y z)) (vec3 (- x) (- y) (- z))])

(define/match (vec3-dot v w)
  [((vec3 vx vy vz) (vec3 wx wy wz)) (+ (* vx wx)
                                        (* vy wy)
                                        (* vz wz))])

(define/match (vec3-cross v w)
  [((vec3 vx vy vz) (vec3 wx wy wz))
   (vec3-sub (vec3-mul (vec3 vy vz vx) (vec3 wz wx wy))
             (vec3-mul (vec3 vz vx vy) (vec3 wy wz wx)))])

(define/match (vec3-length v)
  [((vec3 x y z)) (sqrt (+ (sqr x) (sqr y) (sqr z)))])

(define (vec3-eltwise op v w . more)
  (let ([r (match* (v w)
             [((vec3 vx vy vz) (vec3 wx wy wz)) (vec3 (op vx wx)
                                                      (op vy wy)
                                                      (op vz wz))]
             [((vec3 vx vy vz) (? number?)) (vec3 (op vx w)
                                                  (op vy w)
                                                  (op vz w))]
             [((? number?) (vec3 wx wy wz)) (vec3 (op v wx)
                                                  (op v wy)
                                                  (op v wz))])])
    (if (empty? more)
      r
      (apply vec3-eltwise op r more))))

(define (vec3-add . args) (apply vec3-eltwise + args))
(define (vec3-sub . args) (apply vec3-eltwise - args))
(define (vec3-mul . args) (apply vec3-eltwise * args))
(define (vec3-div . args) (apply vec3-eltwise / args))

(define (vec3-normalize v) (vec3-div v (vec3-length v)))

; ------------------------------------------------------------------------------
; Quaternions.  I use the scalar+vector representation of quaternions, where
; the vector portion gives the coefficients for each of the three basis vectors.

(struct quat (s v) #:transparent)

; The quaternion conjugate, written q* in math.
(define (quat-conjugate q)
  (quat (quat-s q)
        (vec3-negate (quat-v q))))

; Multiplies one quaternion by another.  When using unit quaternions to describe
; rotations, this is equivalent to concatenating two rotations.
(define (quat-mul p q . more)
  (match* (p q)
    [((quat ps pv) (quat qs qv))
     (let ([r (quat ((ps . * . qs) . - . (vec3-dot pv qv))
                    (vec3-add (vec3-cross pv qv)
                              (vec3-add (vec3-mul ps qv)
                                        (vec3-mul qs pv))))])
       (if (empty? more)
         r
         (apply quat-mul r more)))]))

; The quaternion norm.
(define/match (quat-norm q)
  [((quat s v)) (sqrt ((sqr s) . + . (vec3-dot v v)))])

; Normalizes a quaternion to unit length.
(define/match (quat-normalize q)
  [((quat s (vec3 a b c)))
   (let ([n (quat-norm q)])
     (quat (/ s n)
           (vec3 (/ a n)
                 (/ b n)
                 (/ c n))))])

; Rotates a vector by a unit quaternion.
(define (quat-rotate q v)
  (quat-v (quat-mul q (quat 0 v)) (quat-conjugate q)))

; Finds the unit quaternion that describes a rotation from unit vector v
; to unit vector w.
(define (quat-rotation-from-to v w)
  (let ([m (sqrt (2 . + . (2 . * . (vec3-dot v w))))]
        [cr (vec3-cross v w)])
    (quat (m . / . 2)
          (vec3-div cr m))))

(define (quat-rotation-around axis angle)
  (let ([half-angle (angle . / . 2)])
    (quat (cos half-angle) (vec3-mul axis (sin half-angle)))))
