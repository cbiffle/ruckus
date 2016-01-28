#lang racket

; GLSL code generation.  Currently targeting GLSL 1.3 because I can't
; figure out how to switch Racket into 3.3-core.  (Requesting a non-legacy
; GL context crashes.)

(provide
  node->glsl-distance
  node->glsl-disc)

(require "../core/model.rkt")
(require "../core/math.rkt")
(require "../core/compiler/lower.rkt")

(define (wrap str)
  (string-append "(" str ")"))

(define (fn name . args)
  (string-append
    name "("
    (string-join (map glsl-expr args) ", ")
    ")"))

(define (glsl-vec3 x y z)
  (apply fn "vec3" (map (lambda (n) `(cf ,n)) (list x y z))))

(define/match (glsl-quat q)
  [((quat s (vec3 x y z)))
   (apply fn "vec4"
          (map (lambda (n) `(cf ,n)) (list x y z s)))])

(define (glsl-proj v sym)
  (string-append (wrap v) "." (symbol->string sym)))

(define (glsl-choose p a b)
  (string-append (wrap p) " ? " (wrap a) " : " (wrap b)))

(define (bin op a b)
  (string-append (wrap a) " " op " " (wrap b)))

(define (decl t r v)
  (string-append t " r" (number->string r) " = " (glsl-expr v) ";"))

; Generates a GLSL expression from an expression-level intermediate.
(define (glsl-expr form)
  (match form
    [(list 'r n) (string-append "r" (number->string n))]
    [(list 'c3f (list x y z)) (glsl-vec3 x y z)]
    [(list 'c3f (vec3 x y z)) (glsl-vec3 x y z)]
    [(list 'c4f q) (glsl-quat q)]
    [(list 'cf x) (number->string (real->double-flonum x))]
    [(list 'cu x) (string-append (number->string x) "u")]

    [(list 'sub _ a b) (bin "-" (glsl-expr a) (glsl-expr b))]
    [(list 'add _ a b) (bin "+" (glsl-expr a) (glsl-expr b))]
    [(list 'mul _ a b) (bin "*" (glsl-expr a) (glsl-expr b))]
    [(list 'length _ v) (fn "length" v)]
    [(list 'dot _ a b) (fn "dot" a b)]

    [(list '< a b) (bin "<" (glsl-expr a) (glsl-expr b))]
    [(list '> a b) (bin ">" (glsl-expr a) (glsl-expr b))]

    [(list 'abs a) (fn "abs" a)]
    [(list 'max a b) (fn "max" a b)]
    [(list 'min a b) (fn "min" a b)]
    [(list 'smin s a b) (fn "smin" s a b)]
    [(list 'mod a b) (fn "mod" a b)]
    [(list 'qrot q v) (fn "qrot" q v)]
    [(list 'box c p) (fn "dfBox" c p)]
    [(list 'sphere r p) (fn "dfSphere" r p)]
    [(list 'capsule h r p) (fn "dfCapsule" h r p)]
    [(list 'vec3 a b) (fn "vec3" a b)]
    [(list 'vec3 a b c) (fn "vec3" a b c)]
    [(list 'proj _ v sym) (glsl-proj (glsl-expr v) sym)]
    [(list 'radial-project q a s) (fn "radialProject" q a s)]
    [(list 'choose p a b)
     (glsl-choose (glsl-expr p) (glsl-expr a) (glsl-expr b))]
    [_ (error "bad expression passed to glsl-expr: " form)]))

(define (glsl-stmt form)
  (match form
    [(list 'assignf r v) (decl "float" r v)]
    [(list 'assignu r v) (decl "uint" r v)]
    [(list 'assign3f r v) (decl "vec3" r v)]
    [_ (error "bad statement passed to glsl-stmt: " form)]))

(define (node->glsl-distance n)
  (let-values ([(r i s) (generate-statements n)])
    (append (list "float distanceField(vec3 r0) {")
            (for/list ([st (in-list (prune-statements s r))])
              (string-append "  " (glsl-stmt st)))
            (list (string-append "  return r" (number->string r) ";"))
            (list "}")
            )))

(define (node->glsl-disc n)
  (let-values ([(r i s) (generate-statements n)])
    (append (list "uint nearestNodeId(vec3 r0) {")
            (for/list ([st (in-list (prune-statements s i))])
              (string-append "  " (glsl-stmt st)))
            (list (string-append "  return r" (number->string i) ";"))
            (list "}")
            )))

