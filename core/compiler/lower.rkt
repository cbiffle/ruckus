#lang racket

; Lowering to pseudo-assembler.

(provide generate-statements)

(require "../model.rkt")
(require "../math.rkt")
(require "./canon.rkt")
(require "./enumerate.rkt")


(define *statements* (make-parameter #f))
(define *next-value-number* (make-parameter #f))

(define (code form)
  (*statements* (cons form (*statements*))))

(define (fresh-value)
  (let ([v (*next-value-number*)])
    (*next-value-number* (+ 1 v))
    v))

(define (generate node query)
  (case (node-type node)
    [(sphere) (generate-sphere node query)]
    [(half)   (generate-half node query)]
    [(box)   (generate-box node query)]
    [(interpolation-surface) (generate-interpolation-surface node query)]
    [(union root)  (generate-union node query)]
    [(smooth-union)  (generate-smooth-union node query)]
    [(intersection)  (generate-intersection node query)]
    [(inverse) (generate-inverse node query)]
    [(translate)  (generate-translate node query)]
    [(scale)  (generate-scale node query)]
    [(rotate)  (generate-rotate node query)]
    [(extrude) (generate-extrude node query)]
    [(mirror) (generate-mirror node query)]
    [(repeat) (generate-repeat node query)]
    [(iso)  (generate-iso node query)]
    [else     (error "unmatched node type in generate: " (node-type node))]))

;
; Primitive generators
;

(define (generate-sphere node query)
  (let ([r (fresh-value)])
    (code `(assigns ,r (sphere (cs ,(car (node-atts node))) (r ,query))))
    r))

(define (generate-half node query)
  (let ([normal (first (node-atts node))]
        [dist   (second (node-atts node))]
        [d (fresh-value)])
    (code `(assigns ,d (sub 1 (dot 3 (r ,query) (cv ,normal)) (cs ,dist))))
    d))

(define (generate-box node query)
  (let ([corner (vec3-div (first (node-atts node)) 2)]
        [d (fresh-value)])
    (code `(assigns ,d (box (cv ,corner) (r ,query))))
    d))

(define (generate-interpolation-surface node query)
  (define (sum-of-products solution)
    (for/fold ([expr #f])
              ([c (in-list solution)]
              #:when (not (zero? (cdr c))))
      (let ([prod `(mul 1
                        (cs ,(cdr c))
                        (length 3 (sub 3 (r ,query) (cv ,(car c)))))])
        (if expr
          `(add 1 ,expr ,prod)
          prod))))

  (let ([solution (first (node-atts node))]
        [d (fresh-value)])
    (code `(assigns ,d ,(sum-of-products solution)))
    d))

;
; Binary combinators
;

(define ((binary-combinator gen) node query)
  (unless (= 2 (length (node-children node)))
    (error "non-canonical node passed to generate:" (node-type node)))

  (let* ([children (node-children node)]
         [d1 (generate (first children) query)]
         [d2 (generate (second children) query)]
         [d12 (fresh-value)])
    (code `(assigns ,d12
                    ,(apply gen `(r ,d1) `(r ,d2) (node-atts node))))
    d12))

(define generate-union (binary-combinator (lambda (x y) `(min ,x ,y))))

(define generate-smooth-union
  (binary-combinator (lambda (x y smooth)
                       `(smin (cs ,smooth) ,x ,y))))

(define generate-intersection
  (binary-combinator (lambda (x y) `(max ,x ,y))))

(define (generate-iso node query)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical iso passed to generate"))

  (let* ([children (node-children node)]
         [shift (first (node-atts node))]
         [d (generate (first children) query)]
         [d+ (fresh-value)])
    (code `(assigns ,d+ (sub 1 (r ,d) (cs ,shift))))
    d+))

;
; Unary post-transforms
;

(define ((unary-post-combinator gen) node query)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical node passed to generate:" (node-type node)))

  (let* ([children (node-children node)]
         [d (generate (first children) query)]
         [new-d (fresh-value)])
    (code `(assigns ,new-d ,(apply gen `(r ,d) (node-atts node))))
    new-d))

(define generate-inverse
  (unary-post-combinator (lambda (d) `(sub 1 (cs 0) ,d))))


;
; Unary pre-transforms
;

(define ((unary-pre-combinator gen) node query)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical node passed to generate:" (node-type node)))

  (let ([tq (fresh-value)])
    (code `(assignv ,tq ,(apply gen `(r ,query) (node-atts node))))
    (generate (first (node-children node)) tq)))

(define generate-translate
  (unary-pre-combinator (lambda (q v) `(sub 3 ,q (cv ,v)))))

(define generate-rotate
  (unary-pre-combinator (lambda (q r) `(qrot (cq ,r) ,q))))

(define generate-mirror
  (unary-pre-combinator
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

(define ((unary-bracket-combinator pre-gen post-gen) node query)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical node passed to generate:" (node-type node)))

  (let* ([tq (fresh-value)]
         [atts (node-atts node)]
         [child (first (node-children node))]
         [rq `(r ,query)])
    (code `(assignv ,tq ,(apply pre-gen rq atts)))
    (let ([d (generate child tq)]
          [d-corrected (fresh-value)])
      (code `(assigns ,d-corrected ,(apply post-gen rq `(r ,d) atts)))
      d-corrected)))

(define generate-scale
  (unary-bracket-combinator
    (lambda (q scale)
      (let ([scale-inv (map (lambda (n) (1 . / . n)) scale)])
        `(mul 3 ,q (cv ,scale-inv))))
    (lambda (q d scale)
      (let ([correction (apply min scale)])
        `(mul 1 ,d (cs ,correction))))))

(define generate-extrude
  (unary-bracket-combinator
    (lambda (q th)
      `(vec3 (proj 3 ,q x)
             (proj 3 ,q y)
             (cs 0)))
    (lambda (q d th)
      `(max ,d
            (sub 1 (abs (proj 3 ,q z))
                   (cs ,(th . / . 2)))))))

(define (generate-repeat node query)
  ; TODO: this is an awful lot of code for a generator.  Perhaps this node
  ; should be broken up into smaller chunks?  Or perhaps the meat of this
  ; transform should be moved into a GLSL function?
  (unless (= 1 (length (node-children node)))
    (error "non-canonical repeat passed to generate"))

  (let ([children (node-children node)]
        [axis (first (node-atts node))]
        [spacing (list 'cs (second (node-atts node)))]

        ; pq will hold the value number for the periodic query point.
        [pq (fresh-value)]
        ; pq+ and pq- hold the same point, shifted up and down by one interval,
        ; respectively.
        [pq- (fresh-value)]
        [pq+ (fresh-value)])

    ; Populate pq with the query point made periodic over the interval.
    (code `(assignv ,pq
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
    (let* ([z '(cs 0)]  ; shorthand
           [v (case axis
                [(x) `(vec3 ,spacing ,z ,z)]
                [(y) `(vec3 ,z ,spacing ,z)]
                [(z) `(vec3 ,z ,z ,spacing)])])
      (code `(assignv ,pq- (sub 3 (r ,pq) ,v)))
      (code `(assignv ,pq+ (add 3 (r ,pq) ,v))))

    ; Generate child geometry three times, sampling three different points.
    (let ([d (generate (first children) pq)]
          [d- (generate (first children) pq-)]
          [d+ (generate (first children) pq+)]
          [d-result (fresh-value)])
      (code `(assigns ,d-result (min (r ,d) (min (r ,d-) (r ,d+)))))
      d-result)))

(define (generate-statements node)
  (parameterize ([*statements* '()]
                 [*next-value-number* 0])
    (let ([canonical (first (canonicalize node))])
      (let*-values ([(_ enumerated) (enumerate-nodes 0 canonical)]
                    [(r) (generate enumerated (fresh-value))])
        (values r (reverse (*statements*)))))))
