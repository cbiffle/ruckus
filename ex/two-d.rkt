(define (design)
  (rotate '[0 1 0] 45
    (extrude 100
      (difference
        (circle 100)
        (radial-repeat 3
          (at '[55 0 0] (sphere 30)))
        (rect 20 20))))
)
