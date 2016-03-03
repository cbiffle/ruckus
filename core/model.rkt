#lang racket

; Basic data types.

(provide
  (struct-out node)
  node-att-ref
  node-att-set
  node-set-color
  node-color
  node-set-id
  node-id)

(struct node (type atts children) #:transparent)

(define (node-att-ref n k
                      [fail (lambda ()
                              (error "no attribute found with key:" k))])
  (dict-ref (node-atts n) k fail))

(define (node-att-set n k v)
  (struct-copy node n
               [atts (dict-set (node-atts n) k v)]))

; Well known attributes.
(define (node-set-color n c)
  (node-att-set n 'color c))

(define (node-color n)
  (node-att-ref n 'color #f))

(define (node-set-id n i)
  (node-att-set n 'id i))

(define (node-id n)
  (node-att-ref n 'id #f))
