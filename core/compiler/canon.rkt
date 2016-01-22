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
                                                   (rest children)
                                                   #f
                                                   #f)))]))])))

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
                                                   (rest children)
                                                   #f
                                                   #f)))]))])))

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
                                                   (rest children)
                                                   #f
                                                   #f)))]))])))

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
                                                            children
                                                            #f
                                                            #f))]))])))

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
                                                            children
                                                            #f
                                                            #f))]))])))

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
                                                            children
                                                            #f
                                                            #f))]))])))

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
                                                            children
                                                            #f
                                                            #f))]))])))

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
                                                            children
                                                            #f
                                                            #f))]))])))

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
                                                            children
                                                            #f
                                                            #f))]))])))

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
                                                            children
                                                            #f
                                                            #f))]))])))

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
                                                            children
                                                            #f
                                                            #f))]))])))
