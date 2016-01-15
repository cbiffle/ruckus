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
  translate
  rotate
  extrude
  scale
  iso

  mirror-x
  mirror-y
  mirror-z

  repeat-x

  sphere
  rects
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

(define (call-with-translation v body)
  (begin-child 'translate v)  ; TODO use vec3
  (body)
  (end-child))

; Shift child geometry in space.  'v' should be a list of three numbers.
(define-syntax-rule (translate v b bs ...)
  (call-with-translation v (lambda () b bs ...)))

(define (call-with-scale v body)
  (begin-child
    'scale
    (if (number? v)
      (list v v v)
      v))
  (body)
  (end-child))

; Scale child geometry by constant factors.  'v' should either be a list of
; three numbers, giving the scale factors along each axis, or a single number
; to be used for all axes.
(define-syntax-rule (scale v b bs ...)
  (call-with-scale v (lambda () b bs ...)))

(define (call-with-rotation axis angle body)
  (begin-child 'rotate
               (quat-rotation-around (vec3-normalize (apply vec3 axis))
                                     (degrees->radians angle)))
  (body)
  (end-child))

(define-syntax-rule (rotate axis angle b bs ...)
  (call-with-rotation axis angle (lambda () b bs ...)))

(define (call-with-extrusion depth body)
  (begin-child 'extrude depth)
  (body)
  (end-child))

(define-syntax-rule (extrude depth b bs ...)
  (call-with-extrusion depth (lambda () b bs ...)))

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
  (call-with-mirror 'z (lambda () b bs ...)))


(define (call-with-repeat kind period body)
  (begin-child 'repeat kind period)
  (body)
  (end-child))

(define-syntax-rule (repeat-x period b bs ...)
  (call-with-repeat 'x period (lambda () b bs ...)))


; ------------------------------------------------------------------------
; Primitives and basic derived shapes.

(define (sphere radius)
  (add-child 'sphere radius))

(define (half-space p d)
  (add-child 'half p d))

(define (rects sx sy sz)
  (add-child 'box (vec3 sx sy sz)))

(define (cube s)
  (rects s s s))

(define (rect sx sy)
  (add-child 'rect sx sy))

(define (circle r)
  (add-child 'circle r))

(define (interpolation-surface . args)
  (define epsilon 1/100)
  (define (syntax->constraints s)
    (match s
      ; A vector value represents a single, zero-valued constraint.
      [(vec3 _ _ _) (list (cons s 0))]

      ; Two-lists of vectors become triples of constraints along the normal.
      [(list (vec3 _ _ _) (vec3 _ _ _))
       (let ([surf (first s)]
             [norm (vec3-normalize (second s))])
         (list
           (cons surf 0)
           (cons (vec3-add surf (vec3-mul norm epsilon)) epsilon)
           (cons (vec3-sub surf (vec3-mul norm epsilon)) (- epsilon))))]

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
