; Material thickness.
(define th 10)

; Tab width.  Other dimensions are specified in units of tabs.
(define tab (* 2 th))

(unless (> tab th) (error "tab width must be greater than material thickness"))

; Outer box dimensions.
(define width/tab 40)
(define depth/tab 30)
(define height/tab 20)

; Derived dimensions -- don't modify.
(define width (width/tab . * . tab))
(define depth (depth/tab . * . tab))
(define height (height/tab . * . tab))

(define h-width (width . / . 2))
(define h-height (height . / . 2))
(define h-depth (depth . / . 2))
(define h-th (th . / . 2))

(define (design)
  ;(assembly 0)
  (extrude 1000
           (cs/cut-layout))
)

(define (cs/cut-layout)
  (cs/base-top)
  (translate `[0 ,(+ h-height h-depth)] (cs/far-near))
  (translate `[,(+ h-width h-height) 0] (cs/left-right))
  )

(define (assembly ex)
  (union
    (mirror-z
    (translate `[0 0 ,(+ ex (- h-height h-th))]
               (extrude th (cs/base-top))))
    (mirror-y
      (translate `[0 ,(+ ex (- h-depth h-th)) 0]
                 (rotate 90 #:around 'x
                         (extrude th (cs/far-near)))))
    (mirror-x
      (translate `[,(+ ex (- h-width h-th)) 0 0]
                 (rotate 90 #:around 'y
                         (extrude th (cs/left-right)))
                 )
      )))


(define (cs/base-top)
  (difference
    (rect width depth)
    (mirror-y
      (translate `[0 ,h-depth]
                 (notches width/tab #t)))
    (mirror-x
      (translate `[,h-width 0]
                 (rotate 90 (notches depth/tab #t))))
    )
  )

(define (cs/far-near)
  (difference
    (rect width height)
    (mirror-y
      (translate `[0 ,h-height]
                 (notches width/tab #f)))
    (mirror-x
      (translate `[,h-width 0]
                 (rotate 90 (notches height/tab #t))))
    )
  )

(define (cs/left-right)
  (difference
    (rect height depth)
    (mirror-y
      (translate `[0 ,h-depth]
                 (notches height/tab #f)))
    (mirror-x
      (translate `[,h-height 0]
                 (rotate 90 (notches depth/tab #f))))
    )
  )


; Generates an infinite sequence of tab negatives extending along X.
; The negatives are centered around the X axis and extend by 'th' on each
; side to simplify subtraction from panels.
;
; The 'count' parameter is used to distinguish between even and odd tab
; counts, which require a half-tab shift.
;
; 'polarity', when true, shifts the notches by a full tab width.  A part
; with notches cut in 'false' polarity will mate with 'true' polarity.
(define (notches count polarity)
  (let* ([shift1 (if (even? count) (/ tab 2) 0)]
         [shift (if polarity shift1 (+ tab shift1))])
    (translate `[,shift 0]
               (repeat-x (2 . * . tab)
                         (rect tab (* 2 th))))))
