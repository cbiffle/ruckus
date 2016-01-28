#lang racket

; AST canonicalization.

(provide canonicalize)

(require "../model.rkt")
(require "../math.rkt")

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
      [(radial-repeat) (canon-radial-repeat n)]
      [(mirror) (canon-mirror n)]
      [(sphere half box interpolation-surface capsule) (list n)]
      [(rect circle) (canon-2d n)]
      [else (error "unknown node type in canonicalize: " (node-type n))])))

; The root node is rewritten into an implicit union, and is not seen beyond
; this point.
(define (canon-root n)
  (canon-union (struct-copy node n [type 'union])))

; 2D primitives are inflated into 3D primitives with equivalent intersections
; with the XY plane.
(define (canon-2d n)
  (case (node-type n)
    [(rect)
     (let ([w (first (node-atts n))]
           [h (second (node-atts n))])
       (list (node 'box
                   (list (vec3 w h (max w h)))
                   '()
                   (node-id n)
                   (node-color n))))]
    [(circle)
     (let ([r (first (node-atts n))])
       (list (node 'sphere
                   (list r)
                   '()
                   (node-id n)
                   (node-color n))))]
    ))

; Several kinds of nodes canonicalize by
; - collapsing cases of zero or one child,
; - rewriting into a binary tree of like nodes for two or more children.
; These kinds are handled by the binary-canon method.
(define ((binary-canon type) n)
  (let ([children (node-children n)])
    (case (length children)
      [(0) '()]
      [(1) children]
      [(2) (list n)]
      [else (list
              (struct-copy node n
                           [children (cons (first children)
                                           (canonicalize
                                             (node type
                                                   (node-atts n)
                                                   (rest children)
                                                   #f
                                                   #f)))]))])))

(define canon-union        (binary-canon 'union))
(define canon-smooth-union (binary-canon 'smooth-union))
(define canon-intersection (binary-canon 'intersection))

; Difference nodes get rewritten into intersections with inverse volumes.
(define (canon-difference n)
  (define (make-inverse n)
    (node 'inverse
          '()
          (list n)
          #f
          #f))

  (let ([children (node-children n)])
    (canonicalize
      (node 'intersection
            '()
            (apply list (first children) (map make-inverse (rest children)))
            #f
            #f))))

(define (wrap-canon-union children)
  (canon-union (node 'union
                     '()
                     children
                     #f
                     #f)))

(define ((unary-canon merge) n)
  (let ([children (node-children n)])
    (case (length children)
      [(0) '()]
      [(1) (merge n (first children))]
      [else (list (struct-copy node n
                               [children (wrap-canon-union children)]))])))

(define (unary-canon+ type combine-atts no-op?)
  (unary-canon
    (lambda (n child)
      (cond
        [(eq? type (node-type child))
         (canonicalize
           (struct-copy node child [atts (combine-atts (node-atts n)
                                                       (node-atts child))]))]
        [(apply no-op? (node-atts n))
         (list child)]
        [else
          (list n)]))))

; A canonical inverse has a single child.  Inverses of more than one child are
; assumed to be unions of the children and rewritten thus.
;
; Immediately nested inversions cancel.
(define canon-inverse
  (unary-canon
    (lambda (n child)
      (case (node-type child)
        ; Collapse inverse-of-inverse by eliminating both n and child.
        [(inverse) (node-children child)]
        [else (list n)]))))

; A canonical translate has a single child.  Translations of more than one
; child are assumed to be unions of the children and rewritten thus.
;
; Immediately nested translations are flattened by adding their vectors.
;
; Useless translates (translations by zero) are eliminated.
(define canon-translate (unary-canon+
                          'translate
                          (lambda (a b) (list (vec3-add (first a) (first b))))
                          (lambda (v) (equal? (vec3 0 0 0) v))))

; A canonical scale has a single child.  Scalings of more than one child are
; assumed to be unions of the children and rewritten thus.
;
; Immediately nested scalings are flattened by multiplying their factors.
;
; Useless scalings (scalings by 1 on all axes) are eliminated.
(define canon-scale (unary-canon+
                      'scale
                      (lambda (a b) (list (map * (first a) (first b))))
                      (lambda (v) (equal? '(1 1 1) v))))

; A canonical isolevel shift has a single child.  Isolevel shifts of more than
; one child are assumed to be unions of the children and rewritten thus.
;
; Immediately nested isolevel shifts are flattened by adding their sizes.
;
; Useless shifts (shifts by zero) are eliminated.
(define canon-iso (unary-canon+
                    'iso
                    (lambda (a b) (map + a b))
                    (lambda (v) (zero? v))))

; A canonical extrude has a single child.  Extrusions of more than one
; child are assumed to be unions of the children and rewritten thus.
;
; Extrusions don't get merged.
(define canon-extrude (unary-canon (lambda (n _) (list n))))

; A canonical repeat has a single child.  Repetitions of more than one
; child are assumed to be unions of the children and rewritten thus.
(define canon-repeat (unary-canon (lambda (n _) (list n))))

; A canonical radial repeat has a single child.  Repetitions of more than one
; child are assumed to be unions of the children and rewritten thus.
(define canon-radial-repeat (unary-canon (lambda (n _) (list n))))

; A canonical mirror has a single child.  Mirrors of more than one
; child are assumed to be unions of the children and rewritten thus.
;
; The kind attribute must also match the known axes.
(define canon-mirror (unary-canon (lambda (n _) (list n))))

; A canonical rotate has a single child.  Rotations of more than one child are
; assumed to be unions of the children and rewritten thus.
;
; Immediately nested rotations are combined through quaternion multiplication.
;
; Identity rotations are recognized, as long as they're exactly identity, and
; eliminated.
(define canon-rotate (unary-canon+
                       'rotate
                       (lambda (a b) (list (quat-mul (first a) (first b))))
                       (lambda (r) (equal? (quat-identity-rotation) r))))
