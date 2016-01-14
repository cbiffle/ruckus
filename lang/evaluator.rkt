#lang racket

; EDSL script evaluator.

(require "../core/model.rkt")
(require "./state.rkt")

(provide
  call-with-edsl-root)

(define (call-with-edsl-root body)
  (call-with-empty-stack
    (thunk
      (begin-child 'root)
      (body)
      (end-root))))
