#lang ruckus

; This is one of the two parts in the multi-file design.

; Expose part-a to the rest of the design (it is not merely internal).
(provide part-a)

(define (part-a size)
  (cube size))
