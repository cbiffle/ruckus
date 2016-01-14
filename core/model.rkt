#lang racket

(provide
  (struct-out node))

; ------------------------------------------------------------------------
; Basic data types.

(struct node (type atts children) #:transparent)
