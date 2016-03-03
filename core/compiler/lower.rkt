#lang racket

; Lowering to pseudo-assembler.

(provide generate-statements prune-statements)

(require "../model.rkt")
(require "../math.rkt")
(require "./canon.rkt")
(require "./enumerate.rkt")


; ------------------------------------------------------------------------------
; Plumbing in support of generators.

(define *statements* (make-parameter #f))
(define *next-value-number* (make-parameter #f))

(define (code form)
  (*statements* (cons form (*statements*))))

(define (fresh-value)
  (let ([v (*next-value-number*)])
    (*next-value-number* (+ 1 v))
    v))

(define (fresh-value-pair)
  (values (fresh-value) (fresh-value)))

; ------------------------------------------------------------------------------
; Main generator dispatch.

(define (generate node query)
  (case (node-type node)
    [(sphere) (generate-sphere node query)]
    [(half)   (generate-half node query)]
    [(box)   (generate-box node query)]
    [(rect) (generate-rect node query)]
    [(circle) (generate-circle node query)]
    [(interpolation-surface) (generate-interpolation-surface node query)]
    [(capsule) (generate-capsule node query)]
    [(union root)  (generate-union node query)]
    [(smooth-union)  (generate-smooth-union node query)]
    [(intersection)  (generate-intersection node query)]
    [(inverse) (generate-inverse node query)]
    [(translate)  (generate-translate node query)]
    [(scale)  (generate-scale node query)]
    [(rotate)  (generate-rotate node query)]
    [(extrude) (generate-extrude node query)]
    [(slice) (generate-slice node query)]
    [(mirror) (generate-mirror node query)]
    [(repeat) (generate-repeat node query)]
    [(radial-repeat) (generate-radial-repeat node query)]
    [(iso)  (generate-iso node query)]
    [else     (error "unmatched node type in generate: " (node-type node))]))

;
; Primitive generators
;

(define (collect-atts node keys)
  (for/list ([kv keys])
    (if (list? kv)
      (node-att-ref node (first kv) (second kv))
      (node-att-ref node kv))))

(define ((leaf-generator keys gen) node query)
  (let-values ([(d i) (fresh-value-pair)])
    (code `(assignf ,d
                    ,(apply gen `(r ,query)
                            (collect-atts node keys) )))
    (code `(assignu ,i (cu ,(node-id node))))
    (values d i)))

(define generate-sphere
  (leaf-generator '(radius)
    (lambda (query radius)
      `(sphere (cf ,radius) ,query))))

(define generate-capsule
  (leaf-generator '(height radius)
    (lambda (query height radius)
      `(capsule (cf ,(height . / . 2)) (cf ,radius) ,query))))

(define generate-half
  (leaf-generator '(normal distance)
    (lambda (query normal dist)
      `(sub 1 (dot 3 ,query (c3f ,normal)) (cf ,dist)))))

(define generate-box
  (leaf-generator '(size)
    (lambda (query size)
      `(box (c3f ,(vec3-div size 2)) ,query))))

(define generate-rect
  (leaf-generator '(width height)
    (lambda (query w h)
      `(rect (cf ,(w . / . 2)) (cf ,(h . / . 2)) ,query))))

(define generate-circle
  (leaf-generator '(radius)
    (lambda (query radius)
      `(circle (cf ,radius) ,query))))

(define generate-interpolation-surface
  (leaf-generator '(solution)
    (lambda (query solution)
      (for/fold ([expr #f])
                ([c (in-list solution)]
                 #:when (not (zero? (cdr c))))
        (let ([prod `(mul 1
                          (cf ,(cdr c))
                          (length 3 (sub 3 ,query (c3f ,(car c)))))])
          (if expr
            `(add 1 ,expr ,prod)
            prod))))))

; Currently, slice has no impact on code generation, because the actual job of
; collapsing Z is done by extrude.  This may bear revisiting (TODO).
(define (generate-slice node query)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical node passed to generate:" (node-type node)))

  (generate (first (node-children node)) query))


;
; Binary combinators
;

(define ((binary-combinator keys gen-d gen-i) node query)
  (unless (= 2 (length (node-children node)))
    (error "non-canonical node passed to generate:" (node-type node)))

  (let*-values ([(children) (node-children node)]
                [(d1 i1) (generate (first children) query)]
                [(d2 i2) (generate (second children) query)]
                [(d12 i12) (fresh-value-pair)])
    (code `(assignf ,d12
                    ,(apply gen-d `(r ,d1) `(r ,d2)
                            (collect-atts node keys))))
    (code `(assignu ,i12 (choose
                           ,(apply gen-i `(r ,d1) `(r ,d2)
                                   (collect-atts node keys))
                           (r ,i1)
                           (r ,i2))))
    (values d12 i12)))

(define generate-union
  (binary-combinator '()
    (lambda (x y) `(min ,x ,y))
    (lambda (x y) `(,x . < . ,y))))

(define generate-smooth-union
  (binary-combinator '(blend-radius)
    (lambda (x y smooth) `(smin (cf ,smooth) ,x ,y))
    (lambda (x y smooth) `(,x . < . ,y))))

(define generate-intersection
  (binary-combinator '()
    (lambda (x y) `(max ,x ,y))
    (lambda (x y) `(,x . > . ,y))))

;
; Unary post-transforms
;

(define ((unary-post-combinator keys gen) node query)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical node passed to generate:" (node-type node)))

  (let*-values ([(children) (node-children node)]
                [(d i) (generate (first children) query)]
                [(new-d) (fresh-value)])
    (code `(assignf ,new-d ,(apply gen `(r ,d)
                                   (collect-atts node keys))))
    (values new-d i)))

(define generate-inverse
  (unary-post-combinator '() (lambda (d) `(sub 1 (cf 0) ,d))))

(define generate-iso
  (unary-post-combinator '(depth) (lambda (d shift) `(sub 1 ,d (cf ,shift)))))


;
; Unary pre-transforms
;

(define ((unary-pre-combinator keys gen) node query)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical node passed to generate:" (node-type node)))

  (let ([tq (fresh-value)])
    (code `(assign3f ,tq ,(apply gen `(r ,query)
                                 (collect-atts node keys))))
    (generate (first (node-children node)) tq)))

(define generate-translate
  (unary-pre-combinator '(offset) (lambda (q v) `(sub 3 ,q (c3f ,v)))))

(define generate-rotate
  (unary-pre-combinator '(quat) (lambda (q r) `(qrot (c4f ,r) ,q))))

(define generate-mirror
  (unary-pre-combinator '(axis)
    (lambda (q axis)
      (case axis
        [(x) `(vec3 (abs (proj 3 ,q x))
                    (proj 3 ,q y)
                    (proj 3 ,q z))]
        [(y) `(vec3 (proj 3 ,q x)
                    (abs (proj 3 ,q y))
                    (proj 3 ,q z))]
        [(z) `(vec3 (proj 3 ,q x)
                    (proj 3 ,q y)
                    (abs (proj 3 ,q z)))]))))


;
; More complicated, particularly non-isometric, transforms
;

(define ((unary-bracket-combinator pre-keys pre-gen post-keys post-gen)
         node query)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical node passed to generate:" (node-type node)))

  (let* ([tq (fresh-value)]
         [child (first (node-children node))]
         [rq `(r ,query)])
    (code `(assign3f ,tq ,(apply pre-gen rq (collect-atts node pre-keys))))
    (let-values ([(d i) (generate child tq)]
                 [(d-corrected) (fresh-value)])
      (code `(assignf ,d-corrected ,(apply post-gen
                                           rq `(r ,d)
                                           (collect-atts node post-keys))))
      (values d-corrected i))))

(define generate-scale
  (unary-bracket-combinator
    '(factors)
    (lambda (q scale)
      (let ([scale-inv (vec3-div 1 scale)])
        `(mul 3 ,q (c3f ,scale-inv))))
    '(factors)
    (lambda (q d scale)
      (let ([correction (min (vec3-x scale) (vec3-y scale) (vec3-z scale))])
        `(mul 1 ,d (cf ,correction))))))

(define generate-extrude
  (unary-bracket-combinator
    '()
    (lambda (q)
      `(vec3 (proj 3 ,q x)
             (proj 3 ,q y)
             (cf 0)))
    '(depth)
    (lambda (q d th)
      `(max ,d
            (sub 1 (abs (proj 3 ,q z))
                   (cf ,(th . / . 2)))))))

(define (generate-repeat node query)
  ; TODO: this is an awful lot of code for a generator.  Perhaps this node
  ; should be broken up into smaller chunks?  Or perhaps the meat of this
  ; transform should be moved into a GLSL function?
  (unless (= 1 (length (node-children node)))
    (error "non-canonical repeat passed to generate"))

  (let ([children (node-children node)]
        [axis (node-att-ref node 'axis)]
        [spacing (list 'cf (node-att-ref node 'period))]

        ; pq will hold the value number for the periodic query point.
        [pq (fresh-value)]
        ; pq+ and pq- hold the same point, shifted up and down by one interval,
        ; respectively.
        [pq- (fresh-value)]
        [pq+ (fresh-value)])

    ; Populate pq with the query point made periodic over the interval.
    (code `(assign3f ,pq
                     ,(case axis
                        [(x) `(vec3 (mod (proj 3 (r ,query) x) ,spacing)
                                    (proj 3 (r ,query) y)
                                    (proj 3 (r ,query) z))]
                        [(y) `(vec3 (proj 3 (r ,query) x)
                                    (mod (proj 3 (r ,query) y) ,spacing)
                                    (proj 3 (r ,query) z))]
                        [(z) `(vec3 (proj 3 (r ,query) x)
                                    (proj 3 (r ,query) y)
                                    (mod (proj 3 (r ,query) z) ,spacing))])))

    ; Populate pq- with the negatively shifted query point, pq+ with the
    ; positive.
    (let* ([z '(cf 0)]  ; shorthand
           [v (case axis
                [(x) `(vec3 ,spacing ,z ,z)]
                [(y) `(vec3 ,z ,spacing ,z)]
                [(z) `(vec3 ,z ,z ,spacing)])])
      (code `(assign3f ,pq- (sub 3 (r ,pq) ,v)))
      (code `(assign3f ,pq+ (add 3 (r ,pq) ,v))))

    ; Generate child geometry three times, sampling three different points.
    (let-values ([(d i) (generate (first children) pq)]
                 [(d- i-) (generate (first children) pq-)]
                 [(d+ i+) (generate (first children) pq+)]
                 [(d-result i-result) (fresh-value-pair)])
      (code `(assignf ,d-result (min (r ,d) (min (r ,d-) (r ,d+)))))
      (code `(assignu ,i-result
                      (choose ((r ,d-result) . < . (r ,d+))
                              (choose ((r ,d-result) . < . (r ,d-))
                                      (r ,i)
                                      (r ,i-))
                              (r ,i+))))
      (values d-result i-result))))

(define (generate-radial-repeat node query)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical radial-repeat passed to generate"))

  (let* ([children (node-children node)]
         [freq (node-att-ref node 'freq)]
         [angle ((2 . * . pi) . / . freq)]
         ; pq will hold the value number for the periodic query point.
         [pq (fresh-value)]
         ; pq+ and pq- hold the same point, shifted up and down by one interval,
         ; respectively.
         [pq- (fresh-value)]
         [pq+ (fresh-value)])

    ; Populate the query points made periodic over the interval.
    (code `(assign3f ,pq (radial-project (r ,query) (cf ,angle) (cf 0))))
    (code `(assign3f ,pq- (radial-project (r ,query) (cf ,angle)
                                          (cf ,(- angle)))))
    (code `(assign3f ,pq+ (radial-project (r ,query) (cf ,angle)
                                          (cf ,angle))))

    ; Generate child geometry three times, sampling three different points.
    (let-values ([(d i) (generate (first children) pq)]
                 [(d- i-) (generate (first children) pq-)]
                 [(d+ i+) (generate (first children) pq+)]
                 [(d-result i-result) (fresh-value-pair)])
      (code `(assignf ,d-result (min (r ,d) (min (r ,d-) (r ,d+)))))
      (code `(assignu ,i-result
                      (choose ((r ,d-result) . < . (r ,d+))
                              (choose ((r ,d-result) . < . (r ,d-))
                                      (r ,i)
                                      (r ,i-))
                              (r ,i+))))
      (values d-result i-result))))

(define (generate-statements node)
  (parameterize ([*statements* '()]
                 [*next-value-number* 0])
    (let ([canonical (first (canonicalize node))])
      (let*-values ([(_ enumerated) (enumerate-nodes 0 canonical)]
                    [(d i) (generate enumerated (fresh-value))])
        (values d i (reverse (*statements*)))))))

(define (statement-dependencies st)
  (expr-dependencies (third st)))

(define (expr-dependencies e)
  (if (pair? e)
    (if (eq? 'r (first e))
      ; It's a value name; return the corresponding singleton set.
      (set (second e))
      ; Otherwise it's a compound expression.
      (foldl set-union (set) (map expr-dependencies e)))
    ; Not a pair; no contribution.
    (set)))

(define (prune-statements statements result)
  (define/match (helper st)
    [('()) (values '() (set result))]
    [((cons s ss))
     (let-values ([(ss retained) (helper ss)])
       (if (set-member? retained (second s))
         (values (cons s ss)
                 (set-union (statement-dependencies s) retained))
         (values ss retained)))])

  (let-values ([(ss _) (helper statements)])
    ss))
