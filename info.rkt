#lang info
(define version "0.1")
(define collection "ruckus")
(define deps '("base"))

(define scribblings '(("doc/main.scrbl" (multi-page))))
(define compile-omit-paths '("ex"))

(define racket-launcher-libraries
  '("viz/view3d.rkt"
    "viz/shaderdump.rkt"
    "out/outline.rkt"
    "out/surface.rkt"
    "lang/irdump.rkt"
    "lang/rktdump.rkt"
    "lang/astdump.rkt"))
(define racket-launcher-names
  '("ruckus-view"
    "ruckus-dump-glsl"
    "ruckus-export-outline"
    "ruckus-export-surface"
    "ruckus-dump-ir"
    "ruckus-dump-rkt"
    "ruckus-dump-ast"))
