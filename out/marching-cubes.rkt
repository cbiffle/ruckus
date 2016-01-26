#lang racket

; Marching Cubes method for triangulating a surface within a cube of space.
; The implementation follows Paul Bourke's C implementation, which I believe to
; be in the public domain: http://paulbourke.net/geometry/polygonise/

(provide marching-cubes)

(require "../core/math.rkt")

; Triangulates a single cubic domain within a signed distance bound function.
;
; 'f' is the function, from vec3 to real.
;
; 'corner' is a vec3 giving the world coordinates of the most negative corner
; of the domain to triangulate.
;
; 'size' is the dimension, in world units, of the cubic domain.
;
; 'out-fn' will be applied to each triangle found.
(define (marching-cubes f corner size out-fn)
  (let* ([corners (vector corner                                      ; 0
                          (vec3-add corner (vec3 size 0    0))        ; 1
                          (vec3-add corner (vec3 size size 0))        ; 2
                          (vec3-add corner (vec3 0    size 0))        ; 3
                          (vec3-add corner (vec3 0    0    size))     ; 4
                          (vec3-add corner (vec3 size 0    size))     ; 5
                          (vec3-add corner (vec3 size size size))     ; 6
                          (vec3-add corner (vec3 0    size size)))]   ; 7
         [levels (vector-map f corners)]
         [gc (grid-cell corners levels)])
    (polygonize gc out-fn)))

; Describes a cube being triangulated.  'points' and 'values' are both vectors
; of equal length, containing the vertices and field values, respectively.
(struct grid-cell (points values) #:transparent)

; Convenience accessors for getting the point and value by index.
(define (g-point-ref gc idx) (vector-ref (grid-cell-points gc) idx))
(define (g-value-ref gc idx) (vector-ref (grid-cell-values gc) idx))

; For each of 256 possible corner occupancy codes, this table records which of
; the twelve possible edges are involved in producing triangles.  This is used
; to limit the number of interpolations to those necessary for the output.
;
; Each entry is a 12-bit code, where the LSB indicates edge 0 and the MSB edge
; 11.
;
; Edge number N starts at vertex number N, mod 8, and goes to the vertex listed
; at position N in the edge-dests table (below).
;
; The contents of this table are due to Bourke.
(define edge-table
  #(#x0   #x109 #x203 #x30a #x406 #x50f #x605 #x70c
    #x80c #x905 #xa0f #xb06 #xc0a #xd03 #xe09 #xf00
    #x190 #x99  #x393 #x29a #x596 #x49f #x795 #x69c
    #x99c #x895 #xb9f #xa96 #xd9a #xc93 #xf99 #xe90
    #x230 #x339 #x33  #x13a #x636 #x73f #x435 #x53c
    #xa3c #xb35 #x83f #x936 #xe3a #xf33 #xc39 #xd30
    #x3a0 #x2a9 #x1a3 #xaa  #x7a6 #x6af #x5a5 #x4ac
    #xbac #xaa5 #x9af #x8a6 #xfaa #xea3 #xda9 #xca0
    #x460 #x569 #x663 #x76a #x66  #x16f #x265 #x36c
    #xc6c #xd65 #xe6f #xf66 #x86a #x963 #xa69 #xb60
    #x5f0 #x4f9 #x7f3 #x6fa #x1f6 #xff  #x3f5 #x2fc
    #xdfc #xcf5 #xfff #xef6 #x9fa #x8f3 #xbf9 #xaf0
    #x650 #x759 #x453 #x55a #x256 #x35f #x55  #x15c
    #xe5c #xf55 #xc5f #xd56 #xa5a #xb53 #x859 #x950
    #x7c0 #x6c9 #x5c3 #x4ca #x3c6 #x2cf #x1c5 #xcc 
    #xfcc #xec5 #xdcf #xcc6 #xbca #xac3 #x9c9 #x8c0
    #x8c0 #x9c9 #xac3 #xbca #xcc6 #xdcf #xec5 #xfcc
    #xcc  #x1c5 #x2cf #x3c6 #x4ca #x5c3 #x6c9 #x7c0
    #x950 #x859 #xb53 #xa5a #xd56 #xc5f #xf55 #xe5c
    #x15c #x55  #x35f #x256 #x55a #x453 #x759 #x650
    #xaf0 #xbf9 #x8f3 #x9fa #xef6 #xfff #xcf5 #xdfc
    #x2fc #x3f5 #xff  #x1f6 #x6fa #x7f3 #x4f9 #x5f0
    #xb60 #xa69 #x963 #x86a #xf66 #xe6f #xd65 #xc6c
    #x36c #x265 #x16f #x66  #x76a #x663 #x569 #x460
    #xca0 #xda9 #xea3 #xfaa #x8a6 #x9af #xaa5 #xbac
    #x4ac #x5a5 #x6af #x7a6 #xaa  #x1a3 #x2a9 #x3a0
    #xd30 #xc39 #xf33 #xe3a #x936 #x83f #xb35 #xa3c
    #x53c #x435 #x73f #x636 #x13a #x33  #x339 #x230
    #xe90 #xf99 #xc93 #xd9a #xa96 #xb9f #x895 #x99c
    #x69c #x795 #x49f #x596 #x29a #x393 #x99  #x190
    #xf00 #xe09 #xd03 #xc0a #xb06 #xa0f #x905 #x80c
    #x70c #x605 #x50f #x406 #x30a #x203 #x109 #x0))

; For each of the 12 possible edges encoded in each entry of the edge-table,
; this gives the vertex index of the edge's end vertex.
(define edge-dests #(1 2 3 0 5 6 7 4 4 5 6 7))

; For each of the 256 possible corner occupancy codes, names groups of edges
; (by index) whose interpolated positions act as corners of triangles in the
; output.
;
; More specifically, these are vectors of triples of integers.  Each triple
; describes one triangle, using edge numbers (0-11).
;
; The contents of this table are due to Bourke, with some cleanups.
(define tri-table
  (vector #()
          #(#(0 8 3))
          #(#(0 1 9))
          #(#(1 8 3) #(9 8 1))
          #(#(1 2 10))
          #(#(0 8 3) #(1 2 10))
          #(#(9 2 10) #(0 2 9))
          #(#(2 8 3) #(2 10 8) #(10 9 8))
          #(#(3 11 2))
          #(#(0 11 2) #(8 11 0))
          #(#(1 9 0) #(2 3 11))
          #(#(1 11 2) #(1 9 11) #(9 8 11))
          #(#(3 10 1) #(11 10 3))
          #(#(0 10 1) #(0 8 10) #(8 11 10))
          #(#(3 9 0) #(3 11 9) #(11 10 9))
          #(#(9 8 10) #(10 8 11))
          #(#(4 7 8))
          #(#(4 3 0) #(7 3 4))
          #(#(0 1 9) #(8 4 7))
          #(#(4 1 9) #(4 7 1) #(7 3 1))
          #(#(1 2 10) #(8 4 7))
          #(#(3 4 7) #(3 0 4) #(1 2 10))
          #(#(9 2 10) #(9 0 2) #(8 4 7))
          #(#(2 10 9) #(2 9 7) #(2 7 3) #(7 9 4))
          #(#(8 4 7) #(3 11 2))
          #(#(11 4 7) #(11 2 4) #(2 0 4))
          #(#(9 0 1) #(8 4 7) #(2 3 11))
          #(#(4 7 11) #(9 4 11) #(9 11 2) #(9 2 1))
          #(#(3 10 1) #(3 11 10) #(7 8 4))
          #(#(1 11 10) #(1 4 11) #(1 0 4) #(7 11 4))
          #(#(4 7 8) #(9 0 11) #(9 11 10) #(11 0 3))
          #(#(4 7 11) #(4 11 9) #(9 11 10))
          #(#(9 5 4))
          #(#(9 5 4) #(0 8 3))
          #(#(0 5 4) #(1 5 0))
          #(#(8 5 4) #(8 3 5) #(3 1 5))
          #(#(1 2 10) #(9 5 4))
          #(#(3 0 8) #(1 2 10) #(4 9 5))
          #(#(5 2 10) #(5 4 2) #(4 0 2))
          #(#(2 10 5) #(3 2 5) #(3 5 4) #(3 4 8))
          #(#(9 5 4) #(2 3 11))
          #(#(0 11 2) #(0 8 11) #(4 9 5))
          #(#(0 5 4) #(0 1 5) #(2 3 11))
          #(#(2 1 5) #(2 5 8) #(2 8 11) #(4 8 5))
          #(#(10 3 11) #(10 1 3) #(9 5 4))
          #(#(4 9 5) #(0 8 1) #(8 10 1) #(8 11 10))
          #(#(5 4 0) #(5 0 11) #(5 11 10) #(11 0 3))
          #(#(5 4 8) #(5 8 10) #(10 8 11))
          #(#(9 7 8) #(5 7 9))
          #(#(9 3 0) #(9 5 3) #(5 7 3))
          #(#(0 7 8) #(0 1 7) #(1 5 7))
          #(#(1 5 3) #(3 5 7))
          #(#(9 7 8) #(9 5 7) #(10 1 2))
          #(#(10 1 2) #(9 5 0) #(5 3 0) #(5 7 3))
          #(#(8 0 2) #(8 2 5) #(8 5 7) #(10 5 2))
          #(#(2 10 5) #(2 5 3) #(3 5 7))
          #(#(7 9 5) #(7 8 9) #(3 11 2))
          #(#(9 5 7) #(9 7 2) #(9 2 0) #(2 7 11))
          #(#(2 3 11) #(0 1 8) #(1 7 8) #(1 5 7))
          #(#(11 2 1) #(11 1 7) #(7 1 5))
          #(#(9 5 8) #(8 5 7) #(10 1 3) #(10 3 11))
          #(#(5 7 0) #(5 0 9) #(7 11 0) #(1 0 10) #(11 10 0))
          #(#(11 10 0) #(11 0 3) #(10 5 0) #(8 0 7) #(5 7 0))
          #(#(11 10 5) #(7 11 5))
          #(#(10 6 5))
          #(#(0 8 3) #(5 10 6))
          #(#(9 0 1) #(5 10 6))
          #(#(1 8 3) #(1 9 8) #(5 10 6))
          #(#(1 6 5) #(2 6 1))
          #(#(1 6 5) #(1 2 6) #(3 0 8))
          #(#(9 6 5) #(9 0 6) #(0 2 6))
          #(#(5 9 8) #(5 8 2) #(5 2 6) #(3 2 8))
          #(#(2 3 11) #(10 6 5))
          #(#(11 0 8) #(11 2 0) #(10 6 5))
          #(#(0 1 9) #(2 3 11) #(5 10 6))
          #(#(5 10 6) #(1 9 2) #(9 11 2) #(9 8 11))
          #(#(6 3 11) #(6 5 3) #(5 1 3))
          #(#(0 8 11) #(0 11 5) #(0 5 1) #(5 11 6))
          #(#(3 11 6) #(0 3 6) #(0 6 5) #(0 5 9))
          #(#(6 5 9) #(6 9 11) #(11 9 8))
          #(#(5 10 6) #(4 7 8))
          #(#(4 3 0) #(4 7 3) #(6 5 10))
          #(#(1 9 0) #(5 10 6) #(8 4 7))
          #(#(10 6 5) #(1 9 7) #(1 7 3) #(7 9 4))
          #(#(6 1 2) #(6 5 1) #(4 7 8))
          #(#(1 2 5) #(5 2 6) #(3 0 4) #(3 4 7))
          #(#(8 4 7) #(9 0 5) #(0 6 5) #(0 2 6))
          #(#(7 3 9) #(7 9 4) #(3 2 9) #(5 9 6) #(2 6 9))
          #(#(3 11 2) #(7 8 4) #(10 6 5))
          #(#(5 10 6) #(4 7 2) #(4 2 0) #(2 7 11))
          #(#(0 1 9) #(4 7 8) #(2 3 11) #(5 10 6))
          #(#(9 2 1) #(9 11 2) #(9 4 11) #(7 11 4) #(5 10 6))
          #(#(8 4 7) #(3 11 5) #(3 5 1) #(5 11 6))
          #(#(5 1 11) #(5 11 6) #(1 0 11) #(7 11 4) #(0 4 11))
          #(#(0 5 9) #(0 6 5) #(0 3 6) #(11 6 3) #(8 4 7))
          #(#(6 5 9) #(6 9 11) #(4 7 9) #(7 11 9))
          #(#(10 4 9) #(6 4 10))
          #(#(4 10 6) #(4 9 10) #(0 8 3))
          #(#(10 0 1) #(10 6 0) #(6 4 0))
          #(#(8 3 1) #(8 1 6) #(8 6 4) #(6 1 10))
          #(#(1 4 9) #(1 2 4) #(2 6 4))
          #(#(3 0 8) #(1 2 9) #(2 4 9) #(2 6 4))
          #(#(0 2 4) #(4 2 6))
          #(#(8 3 2) #(8 2 4) #(4 2 6))
          #(#(10 4 9) #(10 6 4) #(11 2 3))
          #(#(0 8 2) #(2 8 11) #(4 9 10) #(4 10 6))
          #(#(3 11 2) #(0 1 6) #(0 6 4) #(6 1 10))
          #(#(6 4 1) #(6 1 10) #(4 8 1) #(2 1 11) #(8 11 1))
          #(#(9 6 4) #(9 3 6) #(9 1 3) #(11 6 3))
          #(#(8 11 1) #(8 1 0) #(11 6 1) #(9 1 4) #(6 4 1))
          #(#(3 11 6) #(3 6 0) #(0 6 4))
          #(#(6 4 8) #(11 6 8))
          #(#(7 10 6) #(7 8 10) #(8 9 10))
          #(#(0 7 3) #(0 10 7) #(0 9 10) #(6 7 10))
          #(#(10 6 7) #(1 10 7) #(1 7 8) #(1 8 0))
          #(#(10 6 7) #(10 7 1) #(1 7 3))
          #(#(1 2 6) #(1 6 8) #(1 8 9) #(8 6 7))
          #(#(2 6 9) #(2 9 1) #(6 7 9) #(0 9 3) #(7 3 9))
          #(#(7 8 0) #(7 0 6) #(6 0 2))
          #(#(7 3 2) #(6 7 2))
          #(#(2 3 11) #(10 6 8) #(10 8 9) #(8 6 7))
          #(#(2 0 7) #(2 7 11) #(0 9 7) #(6 7 10) #(9 10 7))
          #(#(1 8 0) #(1 7 8) #(1 10 7) #(6 7 10) #(2 3 11))
          #(#(11 2 1) #(11 1 7) #(10 6 1) #(6 7 1))
          #(#(8 9 6) #(8 6 7) #(9 1 6) #(11 6 3) #(1 3 6))
          #(#(0 9 1) #(11 6 7))
          #(#(7 8 0) #(7 0 6) #(3 11 0) #(11 6 0))
          #(#(7 11 6))
          #(#(7 6 11))
          #(#(3 0 8) #(11 7 6))
          #(#(0 1 9) #(11 7 6))
          #(#(8 1 9) #(8 3 1) #(11 7 6))
          #(#(10 1 2) #(6 11 7))
          #(#(1 2 10) #(3 0 8) #(6 11 7))
          #(#(2 9 0) #(2 10 9) #(6 11 7))
          #(#(6 11 7) #(2 10 3) #(10 8 3) #(10 9 8))
          #(#(7 2 3) #(6 2 7))
          #(#(7 0 8) #(7 6 0) #(6 2 0))
          #(#(2 7 6) #(2 3 7) #(0 1 9))
          #(#(1 6 2) #(1 8 6) #(1 9 8) #(8 7 6))
          #(#(10 7 6) #(10 1 7) #(1 3 7))
          #(#(10 7 6) #(1 7 10) #(1 8 7) #(1 0 8))
          #(#(0 3 7) #(0 7 10) #(0 10 9) #(6 10 7))
          #(#(7 6 10) #(7 10 8) #(8 10 9))
          #(#(6 8 4) #(11 8 6))
          #(#(3 6 11) #(3 0 6) #(0 4 6))
          #(#(8 6 11) #(8 4 6) #(9 0 1))
          #(#(9 4 6) #(9 6 3) #(9 3 1) #(11 3 6))
          #(#(6 8 4) #(6 11 8) #(2 10 1))
          #(#(1 2 10) #(3 0 11) #(0 6 11) #(0 4 6))
          #(#(4 11 8) #(4 6 11) #(0 2 9) #(2 10 9))
          #(#(10 9 3) #(10 3 2) #(9 4 3) #(11 3 6) #(4 6 3))
          #(#(8 2 3) #(8 4 2) #(4 6 2))
          #(#(0 4 2) #(4 6 2))
          #(#(1 9 0) #(2 3 4) #(2 4 6) #(4 3 8))
          #(#(1 9 4) #(1 4 2) #(2 4 6))
          #(#(8 1 3) #(8 6 1) #(8 4 6) #(6 10 1))
          #(#(10 1 0) #(10 0 6) #(6 0 4))
          #(#(4 6 3) #(4 3 8) #(6 10 3) #(0 3 9) #(10 9 3))
          #(#(10 9 4) #(6 10 4))
          #(#(4 9 5) #(7 6 11))
          #(#(0 8 3) #(4 9 5) #(11 7 6))
          #(#(5 0 1) #(5 4 0) #(7 6 11))
          #(#(11 7 6) #(8 3 4) #(3 5 4) #(3 1 5))
          #(#(9 5 4) #(10 1 2) #(7 6 11))
          #(#(6 11 7) #(1 2 10) #(0 8 3) #(4 9 5))
          #(#(7 6 11) #(5 4 10) #(4 2 10) #(4 0 2))
          #(#(3 4 8) #(3 5 4) #(3 2 5) #(10 5 2) #(11 7 6))
          #(#(7 2 3) #(7 6 2) #(5 4 9))
          #(#(9 5 4) #(0 8 6) #(0 6 2) #(6 8 7))
          #(#(3 6 2) #(3 7 6) #(1 5 0) #(5 4 0))
          #(#(6 2 8) #(6 8 7) #(2 1 8) #(4 8 5) #(1 5 8))
          #(#(9 5 4) #(10 1 6) #(1 7 6) #(1 3 7))
          #(#(1 6 10) #(1 7 6) #(1 0 7) #(8 7 0) #(9 5 4))
          #(#(4 0 10) #(4 10 5) #(0 3 10) #(6 10 7) #(3 7 10))
          #(#(7 6 10) #(7 10 8) #(5 4 10) #(4 8 10))
          #(#(6 9 5) #(6 11 9) #(11 8 9))
          #(#(3 6 11) #(0 6 3) #(0 5 6) #(0 9 5))
          #(#(0 11 8) #(0 5 11) #(0 1 5) #(5 6 11))
          #(#(6 11 3) #(6 3 5) #(5 3 1))
          #(#(1 2 10) #(9 5 11) #(9 11 8) #(11 5 6))
          #(#(0 11 3) #(0 6 11) #(0 9 6) #(5 6 9) #(1 2 10))
          #(#(11 8 5) #(11 5 6) #(8 0 5) #(10 5 2) #(0 2 5))
          #(#(6 11 3) #(6 3 5) #(2 10 3) #(10 5 3))
          #(#(5 8 9) #(5 2 8) #(5 6 2) #(3 8 2))
          #(#(9 5 6) #(9 6 0) #(0 6 2))
          #(#(1 5 8) #(1 8 0) #(5 6 8) #(3 8 2) #(6 2 8))
          #(#(1 5 6) #(2 1 6))
          #(#(1 3 6) #(1 6 10) #(3 8 6) #(5 6 9) #(8 9 6))
          #(#(10 1 0) #(10 0 6) #(9 5 0) #(5 6 0))
          #(#(0 3 8) #(5 6 10))
          #(#(10 5 6))
          #(#(11 5 10) #(7 5 11))
          #(#(11 5 10) #(11 7 5) #(8 3 0))
          #(#(5 11 7) #(5 10 11) #(1 9 0))
          #(#(10 7 5) #(10 11 7) #(9 8 1) #(8 3 1))
          #(#(11 1 2) #(11 7 1) #(7 5 1))
          #(#(0 8 3) #(1 2 7) #(1 7 5) #(7 2 11))
          #(#(9 7 5) #(9 2 7) #(9 0 2) #(2 11 7))
          #(#(7 5 2) #(7 2 11) #(5 9 2) #(3 2 8) #(9 8 2))
          #(#(2 5 10) #(2 3 5) #(3 7 5))
          #(#(8 2 0) #(8 5 2) #(8 7 5) #(10 2 5))
          #(#(9 0 1) #(5 10 3) #(5 3 7) #(3 10 2))
          #(#(9 8 2) #(9 2 1) #(8 7 2) #(10 2 5) #(7 5 2))
          #(#(1 3 5) #(3 7 5))
          #(#(0 8 7) #(0 7 1) #(1 7 5))
          #(#(9 0 3) #(9 3 5) #(5 3 7))
          #(#(9 8 7) #(5 9 7))
          #(#(5 8 4) #(5 10 8) #(10 11 8))
          #(#(5 0 4) #(5 11 0) #(5 10 11) #(11 3 0))
          #(#(0 1 9) #(8 4 10) #(8 10 11) #(10 4 5))
          #(#(10 11 4) #(10 4 5) #(11 3 4) #(9 4 1) #(3 1 4))
          #(#(2 5 1) #(2 8 5) #(2 11 8) #(4 5 8))
          #(#(0 4 11) #(0 11 3) #(4 5 11) #(2 11 1) #(5 1 11))
          #(#(0 2 5) #(0 5 9) #(2 11 5) #(4 5 8) #(11 8 5))
          #(#(9 4 5) #(2 11 3))
          #(#(2 5 10) #(3 5 2) #(3 4 5) #(3 8 4))
          #(#(5 10 2) #(5 2 4) #(4 2 0))
          #(#(3 10 2) #(3 5 10) #(3 8 5) #(4 5 8) #(0 1 9))
          #(#(5 10 2) #(5 2 4) #(1 9 2) #(9 4 2))
          #(#(8 4 5) #(8 5 3) #(3 5 1))
          #(#(0 4 5) #(1 0 5))
          #(#(8 4 5) #(8 5 3) #(9 0 5) #(0 3 5))
          #(#(9 4 5))
          #(#(4 11 7) #(4 9 11) #(9 10 11))
          #(#(0 8 3) #(4 9 7) #(9 11 7) #(9 10 11))
          #(#(1 10 11) #(1 11 4) #(1 4 0) #(7 4 11))
          #(#(3 1 4) #(3 4 8) #(1 10 4) #(7 4 11) #(10 11 4))
          #(#(4 11 7) #(9 11 4) #(9 2 11) #(9 1 2))
          #(#(9 7 4) #(9 11 7) #(9 1 11) #(2 11 1) #(0 8 3))
          #(#(11 7 4) #(11 4 2) #(2 4 0))
          #(#(11 7 4) #(11 4 2) #(8 3 4) #(3 2 4))
          #(#(2 9 10) #(2 7 9) #(2 3 7) #(7 4 9))
          #(#(9 10 7) #(9 7 4) #(10 2 7) #(8 7 0) #(2 0 7))
          #(#(3 7 10) #(3 10 2) #(7 4 10) #(1 10 0) #(4 0 10))
          #(#(1 10 2) #(8 7 4))
          #(#(4 9 1) #(4 1 7) #(7 1 3))
          #(#(4 9 1) #(4 1 7) #(0 8 1) #(8 7 1))
          #(#(4 0 3) #(7 4 3))
          #(#(4 8 7))
          #(#(9 10 8) #(10 11 8))
          #(#(3 0 9) #(3 9 11) #(11 9 10))
          #(#(0 1 10) #(0 10 8) #(8 10 11))
          #(#(3 1 10) #(11 3 10))
          #(#(1 2 11) #(1 11 9) #(9 11 8))
          #(#(3 0 9) #(3 9 11) #(1 2 9) #(2 11 9))
          #(#(0 2 11) #(8 0 11))
          #(#(3 2 11))
          #(#(2 3 8) #(2 8 10) #(10 8 9))
          #(#(9 10 2) #(0 9 2))
          #(#(2 3 8) #(2 8 10) #(0 1 8) #(1 10 8))
          #(#(1 10 2))
          #(#(1 3 8) #(9 1 8))
          #(#(0 9 1))
          #(#(0 3 8))
          #()))

; Processes the tables above to triangulate the surface inside grid-cell g.
(define (polygonize g out-fn)
  (let* ([ci (cube-index g)]
         [et (vector-ref edge-table ci)]
         [tte (vector-ref tri-table ci)])
    (unless (zero? et)
      (let ([verts (make-vertices g et)])
        (for ([edges (in-vector tte)])
          (out-fn (list (vector-ref verts (vector-ref edges 0))
                        (vector-ref verts (vector-ref edges 2))
                        (vector-ref verts (vector-ref edges 1)))))))))

; Fill in a 12-vector of interpolated vertices, but only do the actual math for
; entries with a 1 set in edge-table entry 'et'.  Others are stuffed with
; placeholders.
(define (make-vertices g et)
  (build-vector 12
    (lambda (n)
      (if (zero? (et . bitwise-and . (1 . arithmetic-shift . n)))
        #f  ; placeholder
        (let ([start (n . modulo . 8)]
              [end (vector-ref edge-dests n)])
          (vec3-linear-root (g-point-ref g start)
                            (g-value-ref g start)
                            (g-point-ref g end)
                            (g-value-ref g end)))))))

; Computes the corner occupancy code corresponding to the field values in the
; given grid-cell.
(define (cube-index g)
  (for/fold ([idx 0])
            ([v (in-vector (grid-cell-values g))]
             [i (in-range 8)])
    (if (occupied? v)
      (idx . bitwise-ior . (1 . arithmetic-shift . i))
      idx)))

; Centralized occupancy test, so I don't get it backwards in some places.
(define (occupied? v) (negative? v))