#lang racket

; Basic data types.

(provide (struct-out node))

(struct node (type atts children id color) #:transparent)
