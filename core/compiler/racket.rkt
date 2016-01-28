#lang racket

; Racket code generation.

(provide
  node->distance-s-expr
  node->distance-function
  node->disc-s-expr
  node->discriminator)

(require racket/runtime-path)

(require "../model.rkt")
(require "../math.rkt")
(require "../df-prims.rkt")
(require "./canon.rkt")
(require "./enumerate.rkt")
(require "./lower.rkt")

; Produces code for a distance field evaluator lambda for node 'n', as an
; s-expression.
(define (node->distance-s-expr n)
  (let-values ([(r i s) (generate-statements n)])
    `(lambda (r0)
       ,(rkt-fold-statements (prune-statements s r) r))))

; Produces code for a discriminator lambda for node 'n', as an s-expression.
(define (node->disc-s-expr n)
  (let-values ([(r i s) (generate-statements n)])
    `(lambda (r0)
       ,(rkt-fold-statements (prune-statements s i) i))))

; Produces a working distance field evaluator for node 'n'.
(define (node->distance-function n)
  (eval-in-gen-env (node->distance-s-expr n)))

; Produces a working discriminator for node 'n'.
(define (node->discriminator n)
  (eval-in-gen-env (node->disc-s-expr n)))

; Generates a Racket expression from an expression-level intermediate.
(define (rkt-expr form)
  (match form
    [(list 'r n) (string->symbol (string-append "r" (number->string n)))]

    [(list 'c3f (list x y z)) `(vec3 ,x ,y ,z)]
    [(list 'c3f (vec3 x y z)) `(vec3 ,x ,y ,z)]
    [(list 'c4f (quat s (vec3 x y z))) `(quat ,s (vec3 ,x ,y ,z))]
    [(list (or 'cf 'cu) x) x]

    [(list 'sub 1 a b) (rkt-fn '- a b)]
    [(list 'add 1 a b) (rkt-fn '+ a b)]
    [(list 'mul 1 a b) (rkt-fn '* a b)]

    [(list 'sub 3 a b) (rkt-fn 'vec3-sub a b)]
    [(list 'add 3 a b) (rkt-fn 'vec3-add a b)]
    [(list 'mul 3 a b) (rkt-fn 'vec3-mul a b)]
    [(list 'length 3 v) (rkt-fn 'vec3-length v)]
    [(list 'dot 3 a b) (rkt-fn 'vec3-dot a b)]

    [(list 'abs a) (rkt-fn 'abs a)]

    [(list '< a b) (rkt-fn '< a b)]
    [(list '> a b) (rkt-fn '> a b)]
    [(list 'max a b) (rkt-fn 'max a b)]
    [(list 'min a b) (rkt-fn 'min a b)]
    [(list 'smin s a b) (rkt-fn 'smooth-min s a b)]
    [(list 'mod a b) (rkt-fn 'real-mod a b)]
    [(list 'qrot q v) (rkt-fn 'quat-rotate q v)]

    [(list 'choose p a b) (rkt-fn 'if p a b)]

    [(list 'radial-project q a s) (rkt-fn 'radial-project q a s)]

    [(list 'box c p) (rkt-fn 'df-box c p)]
    [(list 'sphere r p) (rkt-fn 'df-sphere r p)]
    [(list 'capsule h r p) (rkt-fn 'df-capsule h r p)]
    [(list 'vec3 a b c) (rkt-fn 'vec3 a b c)]
    [(list 'proj 3 v sym) (rkt-proj (rkt-expr v) sym)] ; TODO
    [_ (error "bad expression passed to rkt-expr: " form)]))

(define (rkt-fn sym . args)
  `(,sym ,@(map rkt-expr args)))

(define (rkt-proj v sym)
  (case sym
    [(x) `(vec3-x ,v)]
    [(y) `(vec3-y ,v)]
    [(z) `(vec3-z ,v)]
    [else (error "Unsupported projection for Racket mode:" sym)]))

(define-runtime-module-path-index mpi-math "../math.rkt")
(define-runtime-module-path-index mpi-df-prims "../df-prims.rkt")
(define eval-modules (list mpi-math mpi-df-prims))

(define (eval-in-gen-env s)
  (let ([ns (make-base-namespace)])
    (namespace-attach-module (current-namespace)
                             (module-path-index-resolve mpi-math)
                             ns)

    (parameterize ([current-namespace ns])
      (namespace-require (module-path-index-resolve mpi-math))
      (namespace-require (resolved-module-path-name (module-path-index-resolve mpi-df-prims)))
      (eval s))))

(define (rkt-fold-statements statements r-final)
  (match statements
    [(cons (list (or 'assignf 'assignu 'assign3f) r v) rest)
     `(let ([,(rkt-expr `(r ,r)) ,(rkt-expr v)])
        ,(rkt-fold-statements rest r-final))]
    ['() (rkt-expr `(r ,r-final))]))
