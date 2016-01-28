#lang racket

; EDSL evaluation internal state.

(require "../core/model.rkt")

(provide
  call-with-empty-stack
  call-with-color
  add-child-node
  begin-child
  end-child
  end-root
  add-child
  current-mode?
  current-mode
  call-with-mode)

; ------------------------------------------------------------------------
; The node stack used during EDSL interpretation and initial AST building.

(define *stack* (make-parameter '()))
(define *color* (make-parameter #f))
(define *mode*  (make-parameter '3d))

(define (call-with-empty-stack body)
  (parameterize ([*stack* '()])
    (body)))

(define (call-with-mode mode body)
  (parameterize ([*mode* mode])
    (body)))

(define (call-with-color c body)
  (parameterize ([*color* c]) (body)))

(define (add-child-node c)
  (unless (node? c) (error "can't add bogus child node"))
  (let* ([s (*stack*)]
         [parent (first s)]
         [parent2 (struct-copy node parent
                               [children (cons c (node-children parent))])])
    (*stack* (cons parent2 (rest s)))))

(define (begin-child type . atts)
  (*stack* (cons (node type atts '() #f (*color*))
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

(define (current-mode)
  (*mode*))

(define (current-mode? mode)
  (eq? mode (*mode*)))
