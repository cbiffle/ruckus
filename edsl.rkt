#lang racket

(require racket/flonum)
(require "model.rkt")

(provide
  union
  sphere
  cube
  half-space
  rects
  translate
  intersection
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

(define (call-as-intersection body)
  (begin-child 'intersection)
  (body)
  (end-child))

(define-syntax-rule (intersection b bs ...)
  (call-as-intersection (lambda () b bs ...)))

(define (call-with-translation v body)
  (begin-child 'translate v)
  (body)
  (end-child))

(define-syntax-rule (translate v b bs ...)
  (call-with-translation v (lambda () b bs ...)))

; ------------------------------------------------------------------------
; Primitives and basic derived shapes.

(define (sphere radius)
  (add-child 'sphere radius))

(define (half-space p d)
  (add-child 'half p d))

(define (rects sx sy sz)
  (intersection
    (half-space '(-1 0 0) 0)
    (half-space '(+1 0 0) sx)
    (half-space '(0 -1 0) 0)
    (half-space '(0 +1 0) sy)
    (half-space '(0 0 -1) 0)
    (half-space '(0 0 +1) sz)))

(define (cube s)
  (let ([shift (- (/ s 2))])
    (translate (list shift shift shift)
               (rects s s s))))
