#lang racket

(provide solve-interpolated-surface-system)

(require math/matrix)
(require "math.rkt")

; An implementation of Turk's algorithm for implicit surfaces that interpolate.
; It isn't clear whether this algorithm can produce predictably
; Lipschitz-continuous results; TBD.

; ------------------------------------------------------------------------------
; Basics of variational interpolation.

; Carr's biharmonic basis function.  I'm using this because it has the right
; shape for a signed distance function.
(define (phi x) (vec3-length x))

; Produces a square matlist of the inter-point phi values for the basic system.
; This also forms the top-left of the augmented system (below).
(define (system-matlist cs)
  (for/list ([ci (in-list cs)])
    (for/list ([cj (in-list cs)])
      (phi (vec3-sub (car ci) (car cj))))))

; Produces a square matrix of the inter-point phi values for the basic system.
(define (system-matrix cs)
  (list*->matrix
      (system-matlist cs)))

; The right-hand side of the equation we need to solve, giving the constraint
; levels for the scalar field.
(define (system-rhs cs)
  (->col-matrix (map cdr cs)))

; Solves the constraint system for the weights, as a matrix.
(define (solve-system-weights cs)
  (matrix-solve (system-matrix cs) (system-rhs cs)))

; Solves the constraint system and produces a list of pairs, each giving a
; constraint point and its corresponding weight.
(define (solve-system cs)
  (let ([weights (solve-system-weights cs)])
    (for/list ([c (in-list cs)]
               [r (in-range (matrix-num-rows weights))])
      (cons (car c) (matrix-ref weights r 0)))))

; Evaluates a point in the interpolated implicit surface characterized by
; 'solution' (the result of 'solve-system').
(define (evaluate-interpolated-surface solution x)
  (for/sum ([c (in-list solution)])
    (* (cdr c)
       (phi (vec3-sub x (car c))))))

; Produces a function that will evaluate the surface defined by 'cs', by
; solving it once and storing the solution.
;
; This is an implicit surface evaluator, *not* a distance field evaluator,
; because the result is not Lipschitz-continuous for L=1.
(define (interpolated-surface-evaluator cs)
  (let ([solution (solve-system cs)])
    (lambda (x)
      (evaluate-interpolated-surface solution x))))

; Produces the skeleton of a function that will evaluate the surface constrained
; by 'cs'.  It's valid Racket code.
(define (interpolated-surface-expr cs)
  (let ([solution (solve-system cs)])
    (cons '+
          (for/list([c (in-list solution)])
            `(* ,(real->double-flonum (cdr c))
                (phi (vec3-sub x ,(car c))))))))

; ------------------------------------------------------------------------------
; Lipschitz compensation, so that the interpolated surface can act as a signed
; distance bound.

(define do-lipschitz-compensation #f)

; Computes the Lipschitz continuity bound L for the solved system described by
; 'solution'.
(define (system-lipschitz-number solution)
  (for/sum ([c (in-list solution)])
    (abs (cdr c))))

; Evaluates a point in the signed distance bound field characterized by
; 'solution' (the result of 'solve-system') and 'L' (the result of
; 'system-lipschitz-number').
(define (evaluate-distance-bound solution L x)
  (/ (evaluate-interpolated-surface solution x) L))

; Produces a function that will evaluate the signed distance bound field of a
; surface constrained by 'cs', by solving it once and stashing the result.
;
; The evaluated function is Lipschitz-continuous for L=1.
(define (distance-bound-evaluator cs)
  (if do-lipschitz-compensation
    (let* ([solution (solve-system cs)]
           [L (system-lipschitz-number solution)])
      (lambda (x)
        (evaluate-distance-bound solution L x)))
    (interpolated-surface-evaluator cs)))

; Produces the skeleton of a function that will evaluate the signed distance
; bound field of the surface constrained by 'cs'.  It's valid Racket code.
(define (distance-bound-expr cs)
  (if do-lipschitz-compensation
    (let* ([solution (solve-system cs)]
           [L (system-lipschitz-number solution)])
      (list '/
            (cons '+
                  (for/list([c (in-list solution)])
                    `(* ,(real->double-flonum (cdr c))
                        (phi (vec3-sub x ,(car c))))))
            (real->double-flonum L)))
    (interpolated-surface-expr cs)))


; ------------------------------------------------------------------------------
; Testing / playground.

; Produce Mathematica code corresponding to a limited Racket expression.
(define (expr->mathematica e)
  (match e
    [(list '* a b) (string-append "("
                                  (expr->mathematica a)
                                  ") * ("
                                  (expr->mathematica b)
                                  ")")]
    [(list '/ a b) (string-append "("
                                  (expr->mathematica a)
                                  ") / ("
                                  (expr->mathematica b)
                                  ")")]
    [(cons '+ es) (string-append "("
                                 (string-join
                                   (map expr->mathematica es)
                                   ") + (")
                                 ")")]
    [(list 'phi r) (string-append "phi["
                                  (expr->mathematica r)
                                  "]")]
    [(list 'vec3-sub a b) (string-append "("
                                         (expr->mathematica a)
                                         ") - ("
                                         (expr->mathematica b)
                                         ")")]
    ['x "{x, y}"]
    [(vec3 x y z) (string-append "{"
                                 (expr->mathematica x)
                                 ", "
                                 (expr->mathematica y)
                                 "}")]
    [(? number?) (number->string (real->double-flonum e))]
    [else (error "bad expr:" e)]))

(define solve-interpolated-surface-system solve-system)

