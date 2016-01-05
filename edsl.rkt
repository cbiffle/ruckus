#lang racket

(require racket/flonum)
(require "model.rkt")
(require "math.rkt")

(provide
  union
  smooth-union
  intersection
  difference
  translate
  rotate
  extrude
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

  call-with-edsl-root)

; ------------------------------------------------------------------------
; The node stack used during EDSL interpretation and initial AST building.

(define *stack* (make-parameter '()))

(define (add-child-node c)
  (unless (node? c) (error "can't add bogus child node"))
  (let* ([s (*stack*)]
         [parent (first s)]
         [parent2 (struct-copy node parent
                               [children (cons c (node-children parent))])])
    (*stack* (cons parent2 (rest s)))))

(define (begin-child type . atts)
  (*stack* (cons (node type atts '())
                 (*stack*))))

(define (end-child)
  (let* ([s (*stack*)]
         [c (first s)])
    (*stack* (rest s))
    (add-child-node (struct-copy node c
                                 [children (reverse (node-children c))]))))

(define (end-root)
  (let* ([s (*stack*)]
         [c (first s)])
    (unless (null? (rest s)) (error "end-root not at bottom of stack"))
    (*stack* (rest s))
    (struct-copy node c
                 [children (reverse (node-children c))])))

(define (add-child type . atts)
  (apply begin-child type atts)
  (end-child))

(define (call-with-edsl-root body)
  (begin-child 'root)
  (body)
  (end-root))

; ------------------------------------------------------------------------
; Combinators.

(define (call-as-union body)
  (begin-child 'union)
  (body)
  (end-child))

(define-syntax-rule (union b bs ...)
  (call-as-union (lambda () b bs ...)))

(define (call-as-smooth-union sm body)
  (begin-child 'smooth-union sm)
  (body)
  (end-child))

(define-syntax-rule (smooth-union sm b bs ...)
  (call-as-smooth-union sm (lambda () b bs ...)))

(define (call-as-intersection body)
  (begin-child 'intersection)
  (body)
  (end-child))

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

(define-syntax-rule (translate v b bs ...)
  (call-with-translation v (lambda () b bs ...)))

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
  (add-child 'box (vec3 (/ sx 2) (/ sy 2) (/ sz 2))))

(define (cube s)
  (rects s s s))

(define (rect sx sy)
  (add-child 'rect sx sy))

(define (circle r)
  (add-child 'circle r))
