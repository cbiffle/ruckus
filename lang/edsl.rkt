#lang racket

; A domain-specific language, embedded in Racket, for constructing two- and
; three-dimensional objects using constructive solid geometry.
;
; This module is implicitly required by any design file, so be careful what
; symbols are provided here!

(require "../core/math.rkt")
(require "../core/model.rkt")
(require "./evaluator.rkt")
(require "./interpolation.rkt")
(require "./state.rkt")

(provide
  union
  smooth-union
  intersection
  difference

  color
  iso

  translate
  at
  rotate
  scale

  extrude
  slice

  mirror-x
  mirror-y
  mirror-z

  repeat-x
  radial-repeat

  sphere
  rects
  capsule
  cube
  half-space
  rect
  circle
  interpolation-surface)

; ------------------------------------------------------------------------
; Combinators.

(define (call-as-union body)
  (begin-child 'union)
  (body)
  (end-child))

; Creates an explicit union of child nodes.  Any point that is on the surface
; of at least one of the children is on the surface of the union of those
; children.
;
; This can be useful for cases where listing the children individually would
; have a different effect, such as inside an 'intersection' node.
;
; Unions are also generated implicitly in some cases, generally when a
; combinator doesn't distinguish its children from one another.
(define-syntax-rule (union b bs ...)
  (call-as-union (lambda () b bs ...)))

(define (call-as-smooth-union sm body)
  (begin-child 'smooth-union sm)
  (body)
  (end-child))

; Creates a smoothed union of child geometry.  This is similar to a 'union',
; but creases where child surfaces would meet are altered so that their radius
; of curvature is not less than 'sm' units.
(define-syntax-rule (smooth-union sm b bs ...)
  (call-as-smooth-union sm (lambda () b bs ...)))

(define (call-as-intersection body)
  (begin-child 'intersection)
  (body)
  (end-child))

; Intersects child nodes.  The result will occupy space that is occupied by
; *all* children.
(define-syntax-rule (intersection b bs ...)
  (call-as-intersection (lambda () b bs ...)))

(define (call-as-difference body)
  (begin-child 'difference)
  (body)
  (end-child))

(define-syntax-rule (difference b bs ...)
  (call-as-difference (lambda () b bs ...)))

(define (call-with-translation called-as v body)
  (define (parse-vector v)
    (match v
      [(list x y z)
       (three-coordinates called-as v)
       (vec3 x y z)]
      [(vec3 _ _ _)
       (three-coordinates called-as v)
       v]
      [(list x y)
       (two-coordinates called-as v)
       (vec3 x y 0)]))   ; TODO: need vec2 type!

  (begin-child 'translate (parse-vector v))
  (body)
  (end-child))

; Shift child geometry in space.
(define-syntax-rule (translate v b bs ...)
  (call-with-translation 'translate v (lambda () b bs ...)))

; Shorthand for translate.
(define-syntax-rule (at v b bs ...)
  (call-with-translation 'at v (lambda () b bs ...)))

(define (call-with-scale v body)
  (define (parse-scale v)
    (match v
      ; Allow a real to be punned for uniform scaling on all axes.
      [(? real? v)
       ; Yes, we produce uniform 3D scaling even in 2D mode, to keep
       ; differently scaled Z extent from interfering with the XY field.
       (vec3 v v v)]
      [(list x y z)
       (three-coordinates 'scale v)
       (vec3 x y z)]
      [(vec3 _ _ _)
       (three-coordinates 'scale v)
       v]
      [(list x y)
       (two-coordinates 'scale v)
       (vec3 x y 0)]))  ; TODO: vec2

  (begin-child 'scale (parse-scale v))
  (body)
  (end-child))

; Scale child geometry by constant factors.  'v' should either be a list of
; three numbers, giving the scale factors along each axis, or a single number
; to be used for all axes.
(define-syntax-rule (scale v b bs ...)
  (call-with-scale v (lambda () b bs ...)))

(define (call-with-rotation axis angle body)
  (let ([axis (match axis
                ['x (vec3 1 0 0)]
                ['y (vec3 0 1 0)]
                ['z (vec3 0 0 1)]
                [(list x y z) (vec3 x y z)]
                [(vec3 _ _ _) axis]
                [else (error "Bad axis for rotate:" axis)])])
    (begin-child 'rotate
                 (quat-rotation-around (vec3-normalize axis)
                                       (degrees->radians angle)))
    (body)
    (end-child)))

(define-syntax-rule (rotate axis angle b bs ...)
  (call-with-rotation axis angle (lambda () b bs ...)))

(define (call-with-extrusion depth body)
  (3d-only 'extrude)
  (begin-child 'extrude depth)
  (call-with-mode '2d body)
  (end-child))

(define-syntax-rule (extrude depth b bs ...)
  (call-with-extrusion depth (lambda () b bs ...)))

(define (call-with-slicing body)
  (2d-only 'slice)
  (begin-child 'slice)
  (call-with-mode '3d body)
  (end-child))

(define-syntax-rule (slice b bs ...)
  (call-with-slicing (lambda () b bs ...)))

(define (call-with-iso depth body)
  (begin-child 'iso depth)
  (body)
  (end-child))

(define-syntax-rule (iso depth b bs ...)
  (call-with-iso depth (lambda () b bs ...)))

(define (call-with-mirror kind body)
  (begin-child 'mirror kind)
  (body)
  (end-child))

(define-syntax-rule (mirror-x b bs ...)
  (call-with-mirror 'x (lambda () b bs ...)))

(define-syntax-rule (mirror-y b bs ...)
  (call-with-mirror 'y (lambda () b bs ...)))

(define-syntax-rule (mirror-z b bs ...)
  (begin
    (when (current-mode? '2d)
      (error "mirror-z cannot be used in a 2D context."))
    (call-with-mirror 'z (lambda () b bs ...))))

(define (call-with-repeat kind period body)
  (begin-child 'repeat kind period)
  (body)
  (end-child))

(define-syntax-rule (repeat-x period b bs ...)
  (call-with-repeat 'x period (lambda () b bs ...)))

(define (call-with-radial-repeat freq body)
  (begin-child 'radial-repeat freq)
  (body)
  (end-child))

(define-syntax-rule (radial-repeat freq b bs ...)
  (call-with-radial-repeat freq (lambda () b bs ...)))

(define-syntax-rule (color c b bs ...)
  (call-with-color c (lambda () b bs ...)))

; ------------------------------------------------------------------------
; Primitives and basic derived shapes.

(define-syntax sphere
  (syntax-rules ()
    [(sphere r) (begin
                  (3d-only 'sphere)
                  (add-child 'sphere r))]
    [(sphere #:radius r) (begin
                           (3d-only 'sphere)
                           (add-child 'sphere r))]
    [(sphere #:diameter d) (begin
                             (3d-only 'sphere)
                             (add-child 'sphere (d . / . 2)))]))

(define (half-space p d)
  (3d-only 'half-space)
  (add-child 'half p d))

(define (rects sx sy sz)
  (3d-only 'rects)
  (add-child 'box (vec3 sx sy sz)))

(define (capsule height radius)
  (3d-only 'capsule)
  (add-child 'capsule height radius))

(define (cube s)
  (3d-only 'cube)  ; Preempt the rects message
  (rects s s s))

(define (rect sx sy)
  (2d-only 'rect)
  (add-child 'rect sx sy))

(define-syntax circle
  (syntax-rules ()
    [(circle r) (begin
                  (2d-only 'circle)
                  (add-child 'circle r))]
    [(circle #:radius r) (begin
                           (2d-only 'circle)
                           (add-child 'circle r))]
    [(circle #:diameter d) (begin
                             (2d-only 'circle)
                             (add-child 'circle d))]))

(define (interpolation-surface args)
  (3d-only 'interpolation-surface)
  (define epsilon 1/100)
  (define (syntax->constraints s)
    (match s
      ; A vector value represents a single, zero-valued constraint.
      [(vec3 _ _ _) (list (cons s 0))]

      ; Two-lists of vectors become triples of constraints along the normal.
      [(list (vec3 _ _ _) (vec3 _ _ _))
       (let* ([surf (first s)]
              [norm (vec3-normalize (second s))]
              [offset (vec3-mul norm epsilon)])
         (list
           (cons surf 0)
           (cons (vec3-add surf offset) epsilon)
           (cons (vec3-sub surf offset) (- epsilon))))]

      ; Allow punning of three-lists for vec3s.
      [(list (? number? x)
             (? number? y)
             (? number? z))
       (syntax->constraints (vec3 x y z))]
      [(list (list (? number? vx)
                   (? number? vy)
                   (? number? vz))
             (list (? number? nx)
                   (? number? ny)
                   (? number? nz)))
       (syntax->constraints (list (vec3 vx vy vz) (vec3 nx ny nz)))]
      [else (error "Bad form as argument to interpolation-surface:" s)]))

  (let* ([cs (apply append (map syntax->constraints args))]
         [solution (solve-interpolated-surface-system cs)])
    (add-child 'interpolation-surface solution)))


; ------------------------------------------------------------------------------
; Utilities for mode-sensitive literal handling.

(define (three-coordinates called-as v)
  (when (current-mode? '2d)
    (raise-argument-error called-as
                          "2d-vector-literal?"
                          v)))

(define (two-coordinates called-as v)
  (when (current-mode? '3d)
    (raise-argument-error called-as
                          "3d-vector-literal?"
                          v)))
  
(define (3d-only called-as)
  (unless (current-mode? '3d)
    (error called-as "cannot be used in a 2D context")))

(define (2d-only called-as)
  (unless (current-mode? '2d)
    (error called-as "cannot be used in a 3D context")))


