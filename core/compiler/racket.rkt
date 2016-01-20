#lang racket

; Racket code generation.

(provide
  node->rkt
  node->function)

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
    [(list 'cf x) x]

    [(list 'sub 1 a b) `(- ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'add 1 a b) `(+ ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'mul 1 a b) `(* ,(rkt-expr a) ,(rkt-expr b))]

    [(list 'sub 3 a b) `(vec3-sub ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'add 3 a b) `(vec3-add ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'mul 3 a b) `(vec3-mul ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'length 3 v) `(vec3-length ,(rkt-expr v))]
    [(list 'dot 3 a b) `(vec3-dot ,(rkt-expr a) ,(rkt-expr b))]

    [(list 'abs a) `(abs ,(rkt-expr a))]

    [(list 'max a b) `(max ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'min a b) `(min ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'smin s a b) `(smooth-min ,(rkt-expr s) ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'mod a b) `(real-mod ,(rkt-expr a) ,(rkt-expr b))]
    [(list 'qrot q v) `(quat-rotate ,(rkt-expr q) ,(rkt-expr v))]

    [(list 'box c p) `(df-box ,(rkt-expr c) ,(rkt-expr p))]
    [(list 'sphere r p) `(df-sphere ,(rkt-expr r) ,(rkt-expr p))]
    [(list 'vec3 a b c) `(vec3 ,(rkt-expr a) ,(rkt-expr b) ,(rkt-expr c))]
    [(list 'proj 3 v sym) (rkt-proj (rkt-expr v) sym)] ; TODO
    [_ (error "bad expression passed to rkt-expr: " form)]))

(define (rkt-proj v sym)
  (case sym
    [(x) `(vec3-x ,v)]
    [(y) `(vec3-y ,v)]
    [(z) `(vec3-z ,v)]
    [else (error "Unsupported projection for Racket mode:" sym)]))

(define (node->rkt n)
  (let-values ([(r s) (generate-statements n)])
    `(lambda (r0) ,(rkt-fold-statements s r))))

(define-runtime-module-path-index mpi-math "../math.rkt")
(define-runtime-module-path-index mpi-df-prims "../df-prims.rkt")
(define eval-modules (list mpi-math mpi-df-prims))

(define (node->function n)
  (let ([ns (make-base-namespace)])
    (for ([m (in-list eval-modules)])
      (namespace-attach-module (current-namespace)
                               (module-path-index-resolve m)
                               ns))

    (parameterize ([current-namespace ns])
      (for ([m (in-list eval-modules)])
        (namespace-require (module-path-index-resolve m)))
      (eval (node->rkt n)))))

(define (rkt-fold-statements statements r-final)
  (match statements
    [(cons (list (or 'assignf 'assign3f) r v) rest)
     `(let ([,(rkt-expr `(r ,r)) ,(rkt-expr v)])
        ,(rkt-fold-statements rest r-final))]
    ['() (rkt-expr `(r ,r-final))]))
