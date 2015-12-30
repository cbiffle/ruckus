#lang racket

; ------------------------------------------------------------------------
; Basic data types.

(struct node (type atts children) #:transparent)

; ------------------------------------------------------------------------
; The node stack used during EDSL interpretation and initial AST building.

(define *stack* (make-parameter '()))

(define (add-child-node c)
  (unless (node? c) (error "can't add bogus child node"))
  (let* ([s (*stack*)]
         [parent (first s)]
         [parent2 (struct-copy node parent
                               [children (cons c (node-children parent))])])
    (*stack* (cons parent2 (rest s)))))


(define (begin-child type . atts)
  (*stack* (cons (node type atts '())
                 (*stack*))))

(define (end-child)
  (let* ([s (*stack*)]
         [c (first s)])
    (*stack* (rest s))
    (add-child-node (struct-copy node c
                                 [children (reverse (node-children c))]))))

(define (end-root)
  (let* ([s (*stack*)]
         [c (first s)])
    (unless (null? (rest s)) (error "end-root not at bottom of stack"))
    (*stack* (rest s))
    (struct-copy node c
                 [children (reverse (node-children c))])))

(define (add-child type . atts)
  (apply begin-child type atts)
  (end-child))

(define (call-as-root body)
  (begin-child 'root)
  (body)
  (end-root))

; ------------------------------------------------------------------------
; Combinators.

(define (call-as-union body)
  (begin-child 'union)
  (body)
  (end-child))

(define-syntax-rule (union b bs ...)
  (call-as-union (lambda () b bs ...)))

(define (call-as-intersection body)
  (begin-child 'intersection)
  (body)
  (end-child))

(define-syntax-rule (intersection b bs ...)
  (call-as-intersection (lambda () b bs ...)))

(define (call-with-translation v body)
  (begin-child 'translate v)
  (body)
  (end-child))

(define-syntax-rule (translate v b bs ...)
  (call-with-translation v (lambda () b bs ...)))

; ------------------------------------------------------------------------
; Primitives and basic derived shapes.

(define (sphere radius)
  (add-child 'sphere radius))

(define (half-space p d)
  (add-child 'half p d))

(define (rects sx sy sz)
  (intersection
    (half-space '(-1 0 0) 0)
    (half-space '(+1 0 0) sx)
    (half-space '(0 -1 0) 0)
    (half-space '(0 +1 0) sy)
    (half-space '(0 0 -1) 0)
    (half-space '(0 0 +1) sz)))

(define (cube s)
  (let ([shift (- (/ s 2))])
    (translate (list shift shift shift)
               (rects s s s))))

(define (test-scene)
  (union
    (sphere 100)
    (translate '[0 150 0] (sphere 50))
    (translate '[125 0 0] (cube 50))
    ))

(call-as-root test-scene)


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
      [(translate) (canon-translate n)]
      [(sphere half) (list n)]
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


; Empty union goes away.
(canonicalize (node 'union '() '()))
; Nested empty unions also go away.
(canonicalize (node 'union '() (list (node 'union '() '()))))
; Union containing a single element reduces.
(canonicalize (node 'union '() (list (node 'sphere '(50) '()))))

(pretty-write (canonicalize (call-as-root test-scene)))

; ----------------------------------------------------------------------
; Lowering to GLSL pseudo-assembler.

(define (code form)
  (pretty-write form))

(define (generate node query rn)
  (case (node-type node)
    [(sphere) (generate-sphere node query rn)]
    [(half)   (generate-half node query rn)]
    [(union root)  (generate-union node query rn)]
    [(intersection)  (generate-intersection node query rn)]
    [(translate)  (generate-translate node query rn)]
    [else     (error "unmatched node type in generate: " (node-type node))]))

(define (generate-sphere node query rn)
  (code `(assign ,rn (- (length (r ,query)) (c ,(car (node-atts node))))))
  (values rn (+ rn 1)))

(define (generate-union node query rn-initial)
  (unless (= 2 (length (node-children node)))
    (error "non-canonical union passed to generate"))

  (let*-values ([(children) (node-children node)]
                [(d1 rn1) (generate (first children) query rn-initial)]
                [(d2 rn2) (generate (second children) query rn1)])
    (code `(assign ,rn2 (min ,d1 ,d2)))
    (values rn2 (+ rn2 1))))

(define (generate-intersection node query rn-initial)
  (unless (= 2 (length (node-children node)))
    (error "non-canonical intersection passed to generate"))

  (let*-values ([(children) (node-children node)]
                [(d1 rn1) (generate (first children) query rn-initial)]
                [(d2 rn2) (generate (second children) query rn1)])
    (code `(assign ,rn2 (max ,d1 ,d2)))
    (values rn2 (+ rn2 1))))

(define (generate-translate node query rn)
  (unless (= 1 (length (node-children node)))
    (error "non-canonical translate passed to generate"))

  (code `(assign ,rn (- (r ,query) (c ,(first (node-atts node))))))
  (generate (first (node-children node)) rn (+ rn 1)))

(define (generate-half node query rn)
  (let ([normal (first (node-atts node))]
        [dist   (second (node-atts node))])
    (code `(assign ,rn (- (dot (r ,query) (c ,normal)) (c ,dist))))
    (values rn (+ rn 1))))

(generate (first (canonicalize (call-as-root test-scene))) 0 1)
