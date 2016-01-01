#lang racket

(require racket/flonum)
(require "model.rkt")
(require "math.rkt")

(provide
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
      [(intersection) (canon-intersection n)]
      [(difference) (canon-difference n)]
      [(inverse) (canon-inverse n)]
      [(translate) (canon-translate n)]
      [(rotate) (canon-rotate n)]
      [(sphere half box) (list n)]
      [else (error "unknown node type in canonicalize: " (node-type n))])))

; The root node is rewritten into an implicit union, and is not seen beyond
; this point.
(define (canon-root n)
  (canon-union (struct-copy node n [type 'union])))

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
      [else (struct-copy node n
                         [children (list (canon-union (node 'union
                                                            '()
                                                            children)))])])))

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
      [else (struct-copy node n
                         [children (list (canon-union (node 'union
                                                            '()
                                                            children)))])])))

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
      [else (struct-copy node n
                         [children (list (canon-union (node 'union
                                                            '()
                                                            children)))])])))


; ----------------------------------------------------------------------
; Lowering to GLSL pseudo-assembler.

(define *statements* (make-parameter '()))

(define (code form)
  (*statements* (cons form (*statements*))))

(define (generate node query rn)
  (case (node-type node)
    [(sphere) (generate-sphere node query rn)]
    [(half)   (generate-half node query rn)]
    [(box)   (generate-box node query rn)]
    [(union root)  (generate-union node query rn)]
    [(intersection)  (generate-intersection node query rn)]
    [(inverse) (generate-inverse node query rn)]
    [(translate)  (generate-translate node query rn)]
    [(rotate)  (generate-rotate node query rn)]
    [else     (error "unmatched node type in generate: " (node-type node))]))

(define (generate-sphere node query rn)
  (code `(assigns ,rn (- (length (r ,query)) (cs ,(car (node-atts node))))))
  (values rn (+ rn 1)))

(define (generate-union node query rn-initial)
  (unless (= 2 (length (node-children node)))
    (error "non-canonical union passed to generate"))

  (let*-values ([(children) (node-children node)]
                [(d1 rn1) (generate (first children) query rn-initial)]
                [(d2 rn2) (generate (second children) query rn1)])
    (code `(assigns ,rn2 (min (r ,d1) (r ,d2))))
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
    (code `(assigns ,rn (- (cs 0) (r ,d))))
    (values rn (+ rn 1))))

(define (generate-translate node query rn)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical translate passed to generate"))

  (code `(assignv ,rn (- (r ,query) (cv ,(first (node-atts node))))))
  (generate (first (node-children node)) rn (+ rn 1)))

(define (generate-rotate node query rn)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical rotate passed to generate"))

  (code `(assignv ,rn (qrot (cq ,(first (node-atts node))) (r ,query))))
  (generate (first (node-children node)) rn (+ rn 1)))

(define (generate-half node query rn)
  (let ([normal (first (node-atts node))]
        [dist   (second (node-atts node))])
    (code `(assigns ,rn (- (dot (r ,query) (cv ,normal)) (cs ,dist))))
    (values rn (+ rn 1))))

(define (generate-box node query rn)
  (let ([corner (first (node-atts node))])
    (code `(assigns ,rn (box (cv ,corner) (r ,query))))
    (values rn (+ rn 1))))

(define (generate-statements node)
  (parameterize ([*statements* '()])
    (let-values ([(r n) (generate (first (canonicalize node)) 0 1)])
      (values r (reverse (*statements*))))))

; ------------------------------------------------------------------------
; GLSL code generation.  Currently targeting GLSL 1.1 because I can't
; figure out how to switch Racket into 3.3-core.  (Requesting a non-legacy
; GL context crashes.)

(define (wrap str)
  (string-append "(" str ")"))

(define (fn name . args)
  (string-append name "(" (string-join args ", ") ")"))

(define (glsl-vec3 x y z)
  (apply fn "vec3" (map number->string (map ->fl (list x y z)))))

(define/match (glsl-quat q)
  [((quat s (vec3 x y z)))
   (apply fn "vec4"
          (map number->string (map real->double-flonum (list x y z s))))])

(define (bin op a b)
  (string-append (wrap a) " " op " " (wrap b)))

(define (decl t r v)
  (string-append t " r" (number->string r) " = " (glsl-expr v) ";"))

; Generates a GLSL expression from an expression-level intermediate.
(define (glsl-expr form)
  (match form
    [(list '- a b) (bin "-" (glsl-expr a) (glsl-expr b))]
    [(list 'r n) (string-append "r" (number->string n))]
    [(list 'cv (list x y z)) (glsl-vec3 x y z)]
    [(list 'cv (vec3 x y z)) (glsl-vec3 x y z)]
    [(list 'cq q) (glsl-quat q)]
    [(list 'cs x) (number->string (->fl x))]
    [(list 'length v) (fn "length" (glsl-expr v))]
    [(list 'dot a b) (fn "dot" (glsl-expr a) (glsl-expr b))]
    [(list 'max a b) (fn "max" (glsl-expr a) (glsl-expr b))]
    [(list 'min a b) (fn "min" (glsl-expr a) (glsl-expr b))]
    [(list 'qrot q v) (fn "qrot" (glsl-expr q) (glsl-expr v))]
    [(list 'box c p) (fn "dfBox" (glsl-expr c) (glsl-expr p))]
    [_ (error "bad expression passed to glsl-expr: " form)]))

(define (glsl-stmt form)
  (match form
    [(list 'assigns r v) (decl "float" r v)]
    [(list 'assignv r v) (decl "vec3" r v)]
    [_ (error "bad statement passed to glsl-stmt: " form)]))

(define (node->glsl n)
  (let-values ([(r s) (generate-statements n)])
    (append (list "float distanceField(vec3 r0) {")
            (map glsl-stmt s)
            (list (string-append "return r" (number->string r) ";"))
            (list "}")
            )))
