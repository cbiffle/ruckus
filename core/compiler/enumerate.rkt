#lang racket

; Enumeration (assigning IDs).

(provide enumerate-nodes)

(require "../model.rkt")

(define (enumerate-nodes next-id n)
  (define (enumerate-children next-id n)
    (for/fold ([id next-id]
               [cc '()])
              ([c (node-children n)])
      (let-values ([(id2 c2) (enumerate-nodes id c)])
        (values id2 (append cc (list c2))))))

  (let*-values ([(next-id children) (enumerate-children next-id n)]
                [(n) (struct-copy node n [children children])])
    (case (node-type n)
      [(sphere half box interpolation-surface capsule)
       (values
         (+ 1 next-id)
         (struct-copy node n
                      [id next-id]))]
      [else (values next-id n)])))


