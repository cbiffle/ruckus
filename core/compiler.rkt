#lang racket

(require racket/runtime-path)

(require "./model.rkt")
(require "./math.rkt")
(require "./df-prims.rkt")

(provide
  generate-statements
  node->rkt
  node->function
  node->glsl)

; ----------------------------------------------------------------------
; AST canonicalization.

(define (canonicalize n)
  (define (canonical-children n)
    (reverse
      (for/fold ([cc '()])
                ([c (node-children n)])
        (append (canonicalize c) cc))))

  (let ([n (struct-copy node n [children (canonical-children n)])])
    (case (node-type n)
      [(root) (canon-root n)]
      [(union) (canon-union n)]
      [(smooth-union) (canon-smooth-union n)]
      [(iso) (canon-iso n)]
      [(intersection) (canon-intersection n)]
      [(difference) (canon-difference n)]
      [(inverse) (canon-inverse n)]
      [(translate) (canon-translate n)]
      [(scale) (canon-scale n)]
      [(rotate) (canon-rotate n)]
      [(extrude) (canon-extrude n)]
      [(repeat) (canon-repeat n)]
      [(mirror) (canon-mirror n)]
      [(sphere half box interpolation-surface) (list n)]
      [(rect circle) (canon-2d n)]
      [else (error "unknown node type in canonicalize: " (node-type n))])))

; The root node is rewritten into an implicit union, and is not seen beyond
; this point.
(define (canon-root n)
  (canon-union (struct-copy node n [type 'union])))

; 2D primitives are inflated into thin 3D primitives.
(define (canon-2d n)
  (case (node-type n)
    [(rect)
     (let ([w (first (node-atts n))]
           [h (second (node-atts n))])
       (list (node 'box (list (vec3 w h (max w h))) '())))]
    [(circle)
     (let ([r (first (node-atts n))])
       (list (node 'sphere (list r) '())))]
    ))

; A canonical union node has exactly two children.  Unions of more than two
; children are rewritten into binary trees.
(define (canon-union n)
  (let ([children (node-children n)])
    (case (length children)
      [(0) '()]
      [(1) children]
      [(2) (list n)]
      [else (list
              (struct-copy node n
                           [children (cons (first children)
                                           (canon-union
                                             (node 'union
                                                   '()
                                                   (rest children))))]))])))

; A canonical smooth union has exactly two children.  Unions of more than two
; children are rewritten into binary trees.
(define (canon-smooth-union n)
  (let ([children (node-children n)])
    (case (length children)
      [(0) '()]
      [(1) children]
      [(2) (list n)]
      [else (list
              (struct-copy node n
                           [children (cons (first children)
                                           (canon-smooth-union
                                             (node 'smooth-union
                                                   (node-atts n)
                                                   (rest children))))]))])))

; A canonical intersection node has exactly two children.  Intersections of
; more than two children are rewritten into binary trees.
(define (canon-intersection n)
  (let ([children (node-children n)])
    (case (length children)
      [(0) '()]
      [(1) children]  ; TODO: or empty?
      [(2) (list n)]
      [else (list
              (struct-copy node n
                           [children (cons (first children)
                                           (canon-intersection
                                             (node 'intersection
                                                   '()
                                                   (rest children))))]))])))

; Difference nodes get rewritten into intersections with inverse volumes.
(define (canon-difference n)
  (define (make-inverse n)
    (node 'inverse '() (list n)))

  (let ([children (node-children n)])
    (canonicalize
      (node 'intersection '()
            (apply list (first children) (map make-inverse (rest children)))))))

; A canonical inverse has a single child.  Inverses of more than one child are
; assumed to be unions of the children and rewritten thus.
;
; Immediately nested inversions cancel.
(define (canon-inverse n)
  (let ([children (node-children n)])
    (case (length children)
      [(0) '()]
      [(1) (case (node-type (first children))
             [(inverse) (node-children (first children))]
             [else (list n)])]
      [else (list (struct-copy node n
                               [children (canon-union (node 'union
                                                            '()
                                                            children))]))])))

; A canonical translate has a single child.  Translations of more than one
; child are assumed to be unions of the children and rewritten thus.
;
; Immediately nested translations are flattened by adding their vectors.
;
; Useless translates (translations by zero) are eliminated.
(define (canon-translate n)
  (define (combine n1 n2)
    (let ([v1 (first (node-atts n1))]
          [v2 (first (node-atts n2))])
      (struct-copy node n2 [atts (list (map + v1 v2))])))

  (let ([children (node-children n)])
    (case (length children)
      [(0) '()]
      [(1) (case (node-type (first children))
             [(translate) (list (combine n (first children)))]
             [else (if (equal? '(0 0 0) (first (node-atts n)))
                     (list (first children))
                     (list n))])]
      [else (list (struct-copy node n
                               [children (canon-union (node 'union
                                                            '()
                                                            children))]))])))

; A canonical scale has a single child.  Scalings of more than one child are
; assumed to be unions of the children and rewritten thus.
;
; Immediately nested scalings are flattened by multiplying their factors.
;
; Useless scalings (scalings by 1 on all axes) are eliminated.
(define (canon-scale n)
  (define (combine n1 n2)
    (let ([v1 (first (node-atts n1))]
          [v2 (first (node-atts n2))])
      (struct-copy node n2 [atts (list (map * v1 v2))])))

  (let ([children (node-children n)])
    (case (length children)
      [(0) '()]
      [(1) (case (node-type (first children))
             [(scale) (list (combine n (first children)))]
             [else (if (equal? '(1 1 1) (first (node-atts n)))
                     (list (first children))
                     (list n))])]
      [else (list (struct-copy node n
                               [children (canon-union (node 'union
                                                            '()
                                                            children))]))])))

; A canonical isolevel shift has a single child.  Isolevel shifts of more than
; one child are assumed to be unions of the children and rewritten thus.
;
; Immediately nested isolevel shifts are flattened by adding their sizes.
;
; Useless shifts (shifts by zero) are eliminated.
(define (canon-iso n)
  (define (combine n1 n2)
    (let ([d1 (first (node-atts n1))]
          [d2 (first (node-atts n2))])
      (struct-copy node n2 [atts (list (+ d1 d2))])))

  (let ([children (node-children n)])
    (case (length children)
      [(0) '()]
      [(1) (case (node-type (first children))
             [(iso) (list (combine n (first children)))]
             [else (if (zero? (first (node-atts n)))
                     (list (first children))
                     (list n))])]
      [else (list (struct-copy node n
                               [children (canon-union (node 'union
                                                            '()
                                                            children))]))])))

; A canonical extrude has a single child.  Extrusions of more than one
; child are assumed to be unions of the children and rewritten thus.
(define (canon-extrude n)
  (let ([children (node-children n)])
    (case (length children)
      [(0) '()]
      [(1) (list n)]
      [else (list (struct-copy node n
                               [children (canon-union (node 'union
                                                            '()
                                                            children))]))])))

; A canonical repeat has a single child.  Repetitions of more than one
; child are assumed to be unions of the children and rewritten thus.
(define (canon-repeat n)
  (let ([children (node-children n)])
    (case (length children)
      [(0) '()]
      [(1) (list n)]
      [else (list (struct-copy node n
                               [children (canon-union (node 'union
                                                            '()
                                                            children))]))])))

; A canonical mirror has a single child.  Mirrors of more than one
; child are assumed to be unions of the children and rewritten thus.
;
; The kind attribute must also match the known axes.
(define (canon-mirror n)
  (let ([children (node-children n)]
        [axis (first (node-atts n))])
    (unless (member axis '(x y z))
      (error "bad axis used in mirror: " axis))

    (case (length children)
      [(0) '()]
      [(1) (list n)]
      [else (list (struct-copy node n
                               [children (canon-union (node 'union
                                                            '()
                                                            children))]))])))

; A canonical rotate has a single child.  Rotations of more than one child are
; assumed to be unions of the children and rewritten thus.
;
; Immediately nested rotations are combined through quaternion multiplication.
(define (canon-rotate n)
  (define (combine n1 n2)
    (let ([q1 (first (node-atts n1))]
          [q2 (first (node-atts n2))])
      (struct-copy node n2 [atts (list (quat-mul q1 q2))])))

  (let ([children (node-children n)])
    (case (length children)
      [(0) '()]
      [(1) (case (node-type (first children))
             [(rotate) (list (combine n (first children)))]
             [else (list n)])]
      [else (list (struct-copy node n
                               [children (canon-union (node 'union
                                                            '()
                                                            children))]))])))


; ----------------------------------------------------------------------
; Lowering to pseudo-assembler.

(define *statements* (make-parameter '()))

(define (code form)
  (*statements* (cons form (*statements*))))

(define (generate node query rn)
  (case (node-type node)
    [(sphere) (generate-sphere node query rn)]
    [(half)   (generate-half node query rn)]
    [(box)   (generate-box node query rn)]
    [(interpolation-surface) (generate-interpolation-surface node query rn)]
    [(union root)  (generate-union node query rn)]
    [(smooth-union)  (generate-smooth-union node query rn)]
    [(intersection)  (generate-intersection node query rn)]
    [(inverse) (generate-inverse node query rn)]
    [(translate)  (generate-translate node query rn)]
    [(scale)  (generate-scale node query rn)]
    [(rotate)  (generate-rotate node query rn)]
    [(extrude) (generate-extrude node query rn)]
    [(mirror) (generate-mirror node query rn)]
    [(repeat) (generate-repeat node query rn)]
    [(iso)  (generate-iso node query rn)]
    [else     (error "unmatched node type in generate: " (node-type node))]))

(define (generate-sphere node query rn)
  (code `(assigns ,rn (sphere (cs ,(car (node-atts node))) (r ,query))))
  (values rn (+ rn 1)))

(define (generate-union node query rn-initial)
  (unless (= 2 (length (node-children node)))
    (error "non-canonical union passed to generate"))

  (let*-values ([(children) (node-children node)]
                [(d1 rn1) (generate (first children) query rn-initial)]
                [(d2 rn2) (generate (second children) query rn1)])
    (code `(assigns ,rn2 (min (r ,d1) (r ,d2))))
    (values rn2 (+ rn2 1))))

(define (generate-iso node query rn-initial)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical iso passed to generate"))

  (let*-values ([(children) (node-children node)]
                [(shift) (first (node-atts node))]
                [(d rn1) (generate (first children) query rn-initial)])
    (code `(assigns ,rn1 (sub 1 (r ,d) (cs ,shift))))
    (values rn1 (+ rn1 1))))

(define (generate-smooth-union node query rn-initial)
  (unless (= 2 (length (node-children node)))
    (error "non-canonical smooth-union passed to generate"))

  (let*-values ([(children) (node-children node)]
                [(smooth) (first (node-atts node))]
                [(d1 rn1) (generate (first children) query rn-initial)]
                [(d2 rn2) (generate (second children) query rn1)])
    (code `(assigns ,rn2 (smin (cs ,smooth) (r ,d1) (r ,d2))))
    (values rn2 (+ rn2 1))))

(define (generate-intersection node query rn-initial)
  (unless (= 2 (length (node-children node)))
    (error "non-canonical intersection passed to generate"))

  (let*-values ([(children) (node-children node)]
                [(d1 rn1) (generate (first children) query rn-initial)]
                [(d2 rn2) (generate (second children) query rn1)])
    (code `(assigns ,rn2 (max (r ,d1) (r ,d2))))
    (values rn2 (+ rn2 1))))

(define (generate-inverse node query rn-initial)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical inverse passed to generate"))

  (let*-values ([(children) (node-children node)]
                [(d rn) (generate (first children) query rn-initial)])
    (code `(assigns ,rn (sub 1 (cs 0) (r ,d))))
    (values rn (+ rn 1))))

(define (generate-translate node query rn)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical translate passed to generate"))

  (code `(assignv ,rn (sub 3 (r ,query) (cv ,(first (node-atts node))))))
  (generate (first (node-children node)) rn (+ rn 1)))

(define (generate-scale node query rn)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical scale passed to generate"))

  ; Bookkeeping:
  ; - 'scale' is the 3-list of scale factors ordered by axis.
  ; - 'scale-inv' is the inverse, by which we multiply the query point.
  ; - 'correction' is the Lipschitz factor correction that must be applied to
  ;   the result to maintain L=1 Lipschitz continuity.
  (let* ([scale (first (node-atts node))]
         [scale-inv (map (lambda (n) (/ 1 n)) scale)]
         [correction (apply min scale)])
    ; Generate the scaled query point.
    (code `(assignv ,rn (mul 3 (r ,query) (cv ,scale-inv))))
    ; Evaluate the child's distance field.
    (let-values ([(d rn2) (generate (first (node-children node))
                                    rn
                                    (+ rn 1))])
      ; Apply Lipschitz correction.
      (code `(assigns ,rn2 (mul 1 (r ,d) (cs ,correction))))
      (values rn2 (+ rn2 1)))))

(define (generate-extrude node query rn0)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical extrude passed to generate"))

  ; Populate rn with the projection of the query point onto the XY plane.
  (code `(assignv ,rn0 (vec3 (proj 3 (r ,query) x)
                             (proj 3 (r ,query) y)
                             (cs 0))))

  (let ([children (node-children node)]
        [th-sym (/ (first (node-atts node)) 2)]
        [rn1 (+ rn0 1)])
    (let-values ([(d rn2) (generate (first children)
                                    rn0
                                    rn1)])
      (code `(assigns ,rn2 (max (r ,d)
                                (sub 1 (abs (proj 3 (r ,query) z))
                                     (cs ,th-sym)))))
      (values rn2 (+ rn2 1)))))

(define (generate-mirror node query rn0)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical mirror passed to generate"))

  ; Populate rn with the reflection of the query point into the positive
  ; side of the mirror axis space.
  (case (first (node-atts node))
    [(x) (code `(assignv ,rn0 (vec3 (abs (proj 3 (r ,query) x))
                                    (proj 3 (r ,query) y)
                                    (proj 3 (r ,query) z))))]
    [(y) (code `(assignv ,rn0 (vec3 (proj 3 (r ,query) x)
                                    (abs (proj 3 (r ,query) y))
                                    (proj 3 (r ,query) z))))]
    [(z) (code `(assignv ,rn0 (vec3 (proj 3 (r ,query) x)
                                    (proj 3 (r ,query) y)
                                    (abs (proj 3 (r ,query) z)))))])

  (let* ([children (node-children node)]
         [rn1 (+ rn0 1)])
    (generate (first children) rn0 rn1)))

(define (generate-repeat node query rn0)
  ; TODO: this is an awful lot of code for a generator.  Perhaps this node
  ; should be broken up into smaller chunks?  Or perhaps the meat of this
  ; transform should be moved into a GLSL function?
  (unless (= 1 (length (node-children node)))
    (error "non-canonical repeat passed to generate"))

  (let ([children (node-children node)]
        [axis (first (node-atts node))]
        [spacing (list 'cs (second (node-atts node)))])

    ; Populate rn0 with the query point made periodic over the interval.
    (case axis
      [(x) (code `(assignv ,rn0 (vec3 (mod (proj 3 (r ,query) x) ,spacing)
                                      (proj 3 (r ,query) y)
                                      (proj 3 (r ,query) z))))]
      [(y) (code `(assignv ,rn0 (vec3 (proj 3 (r ,query) x)
                                      (mod (proj 3 (r ,query) y) ,spacing)
                                      (proj 3 (r ,query) z))))]
      [(z) (code `(assignv ,rn0 (vec3 (proj 3 (r ,query) x)
                                      (proj 3 (r ,query) y)
                                      (mod (proj 3 (r ,query) z) ,spacing))))])

    ; Populate rn1 with the negatively shifted query point, rn2 with the
    ; positive.
    (let ([z (list 'cs 0)])
      (case axis
        [(x)
         (code `(assignv ,(+ rn0 1) (sub 3 (r ,rn0) (vec3 ,spacing ,z ,z))))
         (code `(assignv ,(+ rn0 2) (add 3 (r ,rn0) (vec3 ,spacing ,z ,z))))]
        [(y)
         (code `(assignv ,(+ rn0 1) (sub 3 (r ,rn0) (vec3 ,z ,spacing ,z))))
         (code `(assignv ,(+ rn0 2) (add 3 (r ,rn0) (vec3 ,z ,spacing ,z))))]
        [(z)
         (code `(assignv ,(+ rn0 1) (sub 3 (r ,rn0) (vec3 ,z ,z ,spacing))))
         (code `(assignv ,(+ rn0 2) (add 3 (r ,rn0) (vec3 ,z ,z ,spacing))))]))

    (let*-values ([(dc rn3) (generate (first children) rn0 (+ rn0 3))]
                  [(dm rn4) (generate (first children) (+ rn0 1) rn3)]
                  [(dp rn5) (generate (first children) (+ rn0 2) rn4)])
      (code `(assigns ,rn5 (min (r ,dc) (min (r ,dm) (r ,dp)))))
      (values rn5 (+ rn5 1)))))

(define (generate-rotate node query rn)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical rotate passed to generate"))

  (code `(assignv ,rn (qrot (cq ,(first (node-atts node))) (r ,query))))
  (generate (first (node-children node)) rn (+ rn 1)))

(define (generate-half node query rn)
  (let ([normal (first (node-atts node))]
        [dist   (second (node-atts node))])
    (code `(assigns ,rn (sub 1 (dot 3 (r ,query) (cv ,normal)) (cs ,dist))))
    (values rn (+ rn 1))))

(define (generate-box node query rn)
  (let ([corner (vec3-div (first (node-atts node)) 2)])
    (code `(assigns ,rn (box (cv ,corner) (r ,query))))
    (values rn (+ rn 1))))

(define (generate-interpolation-surface node query rn)
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

  (let ([solution (first (node-atts node))])
    (code `(assigns ,rn ,(sum-of-products solution)))
    (values rn (+ rn 1))))

(define (generate-statements node)
  (parameterize ([*statements* '()])
    (let-values ([(r n) (generate (first (canonicalize node)) 0 1)])
      (values r (reverse (*statements*))))))

; ------------------------------------------------------------------------
; GLSL code generation.  Currently targeting GLSL 1.3 because I can't
; figure out how to switch Racket into 3.3-core.  (Requesting a non-legacy
; GL context crashes.)

(define (wrap str)
  (string-append "(" str ")"))

(define (fn name . args)
  (string-append name "(" (string-join args ", ") ")"))

(define (glsl-vec3 x y z)
  (apply fn "vec3" (map number->string (map real->double-flonum (list x y z)))))

(define/match (glsl-quat q)
  [((quat s (vec3 x y z)))
   (apply fn "vec4"
          (map number->string (map real->double-flonum (list x y z s))))])

(define (glsl-proj v sym)
  (string-append (wrap v) "." (symbol->string sym)))

(define (bin op a b)
  (string-append (wrap a) " " op " " (wrap b)))

(define (decl t r v)
  (string-append t " r" (number->string r) " = " (glsl-expr v) ";"))

; Generates a GLSL expression from an expression-level intermediate.
(define (glsl-expr form)
  (match form
    [(list 'r n) (string-append "r" (number->string n))]
    [(list 'cv (list x y z)) (glsl-vec3 x y z)]
    [(list 'cv (vec3 x y z)) (glsl-vec3 x y z)]
    [(list 'cq q) (glsl-quat q)]
    [(list 'cs x) (number->string (real->double-flonum x))]

    [(list 'sub _ a b) (bin "-" (glsl-expr a) (glsl-expr b))]
    [(list 'add _ a b) (bin "+" (glsl-expr a) (glsl-expr b))]
    [(list 'mul _ a b) (bin "*" (glsl-expr a) (glsl-expr b))]
    [(list 'length _ v) (fn "length" (glsl-expr v))]
    [(list 'dot _ a b) (fn "dot" (glsl-expr a) (glsl-expr b))]

    [(list 'abs a) (fn "abs" (glsl-expr a))]
    [(list 'max a b) (fn "max" (glsl-expr a) (glsl-expr b))]
    [(list 'min a b) (fn "min" (glsl-expr a) (glsl-expr b))]
    [(list 'smin s a b) (fn "smin" (glsl-expr s) (glsl-expr a) (glsl-expr b))]
    [(list 'mod a b) (fn "mod" (glsl-expr a) (glsl-expr b))]
    [(list 'qrot q v) (fn "qrot" (glsl-expr q) (glsl-expr v))]
    [(list 'box c p) (fn "dfBox" (glsl-expr c) (glsl-expr p))]
    [(list 'sphere r p) (fn "dfSphere" (glsl-expr r) (glsl-expr p))]
    [(list 'vec3 a b) (fn "vec3" (glsl-expr a) (glsl-expr b))]
    [(list 'vec3 a b c) (fn "vec3" (glsl-expr a) (glsl-expr b) (glsl-expr c))]
    [(list 'proj _ v sym) (glsl-proj (glsl-expr v) sym)]
    [_ (error "bad expression passed to glsl-expr: " form)]))

(define (glsl-stmt form)
  (match form
    [(list 'assigns r v) (decl "float" r v)]
    [(list 'assignv r v) (decl "vec3" r v)]
    [_ (error "bad statement passed to glsl-stmt: " form)]))

(define (node->glsl n)
  (let-values ([(r s) (generate-statements n)])
    (append (list "float distanceField(vec3 r0) {")
            (map (lambda (st) (string-append "  " (glsl-stmt st)))
                 s)
            (list (string-append "  return r" (number->string r) ";"))
            (list "}")
            )))

; -----------------------------------------------------------------------------
; Racket code generation.

; Generates a Racket expression from an expression-level intermediate.
(define (rkt-expr form)
  (match form
    [(list 'r n) (string->symbol (string-append "r" (number->string n)))]

    [(list 'cv (list x y z)) `(vec3 ,x ,y ,z)]
    [(list 'cv (vec3 x y z)) `(vec3 ,x ,y ,z)]
    [(list 'cq (quat s (vec3 x y z))) `(quat ,s (vec3 ,x ,y ,z))]
    [(list 'cs x) x]

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

(define-runtime-module-path-index mpi-math "./math.rkt")
(define-runtime-module-path-index mpi-df-prims "./df-prims.rkt")
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
    [(cons (list (or 'assigns 'assignv) r v) rest)
     `(let ([,(rkt-expr `(r ,r)) ,(rkt-expr v)])
        ,(rkt-fold-statements rest r-final))]
    [(cons (list 'assigns r v) rest)
     `(let ([,(rkt-expr `(r ,r)) ,(rkt-expr v)])
        ,(rkt-fold-statements rest r-final))]
    ['() (rkt-expr `(r ,r-final))]))

