#lang ruckus

; This demonstrates a multi-file design using 'require' and 'provide'.
; This file is the top-level of the design; it just requires other files
; and then assembles their parts.

(require "part-a.rkt")
(require "part-b.rkt")

(part-a 170)
(part-b 100)
