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

; Generates a Racket expression from an expression-level intermediate.
(define (rkt-expr form)
  (match form
    [(list 'r n) (string->symbol (string-append "r" (number->string n)))]

    [(list 'c3f (list x y z)) `(vec3 ,x ,y ,z)]
    [(list 'c3f (vec3 x y z)) `(vec3 ,x ,y ,z)]
    [(list 'c4f (quat s (vec3 x y z))) `(quat ,s (vec3 ,x ,y ,z))]
    [(list (or 'cf 'cu) x) x]

    [(list 'sub 1 a b) `(- ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'add 1 a b) `(+ ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'mul 1 a b) `(* ,(rkt-expr a) ,(rkt-expr b))]

    [(list 'sub 3 a b) `(vec3-sub ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'add 3 a b) `(vec3-add ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'mul 3 a b) `(vec3-mul ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'length 3 v) `(vec3-length ,(rkt-expr v))]
    [(list 'dot 3 a b) `(vec3-dot ,(rkt-expr a) ,(rkt-expr b))]

    [(list 'abs a) `(abs ,(rkt-expr a))]

    [(list '< a b) `(< ,(rkt-expr a) ,(rkt-expr b))]
    [(list '> a b) `(> ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'max a b) `(max ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'min a b) `(min ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'smin s a b) `(smooth-min ,(rkt-expr s) ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'mod a b) `(real-mod ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'qrot q v) `(quat-rotate ,(rkt-expr q) ,(rkt-expr v))]

    [(list 'choose p a b) `(if ,(rkt-expr p) ,(rkt-expr a) ,(rkt-expr b))]

    [(list 'box c p) `(df-box ,(rkt-expr c) ,(rkt-expr p))]
    [(list 'sphere r p) `(df-sphere ,(rkt-expr r) ,(rkt-expr p))]
    [(list 'capsule h r p) `(df-capsule ,(rkt-expr h)
                                        ,(rkt-expr r)
                                        ,(rkt-expr p))]
    [(list 'vec3 a b c) `(vec3 ,(rkt-expr a) ,(rkt-expr b) ,(rkt-expr c))]
    [(list 'proj 3 v sym) (rkt-proj (rkt-expr v) sym)] ; TODO
    [_ (error "bad expression passed to rkt-expr: " form)]))

(define (rkt-proj v sym)
  (case sym
    [(x) `(vec3-x ,v)]
    [(y) `(vec3-y ,v)]
    [(z) `(vec3-z ,v)]
    [else (error "Unsupported projection for Racket mode:" sym)]))

(define (node->distance-s-expr n)
  (let-values ([(r i s) (generate-statements n)])
    `(lambda (r0)
       ,(rkt-fold-statements (prune-statements s r) r))))

(define (node->disc-s-expr n)
  (let-values ([(r i s) (generate-statements n)])
    `(lambda (r0)
       ,(rkt-fold-statements (prune-statements s i) i))))

(define (node->distance-function n)
  (eval-in-gen-env (node->distance-s-expr n)))

(define (node->discriminator n)
  (eval-in-gen-env (node->disc-s-expr n)))

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
