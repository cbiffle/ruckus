#lang ruckus

; This is one of the two parts in the multi-file design.

; Expose part-b to the rest of the design (it is not merely internal).
(provide part-b)

(define (part-b size)
  (sphere size))
