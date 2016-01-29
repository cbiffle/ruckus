#lang racket

(provide image+ (rename-out [image+ image_]))

(require racket/runtime-path)
(require scribble/base)

(define-syntax-rule (image+ path)
  (begin
    (define-runtime-path the-path path)
    (image the-path)))
