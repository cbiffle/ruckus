(define (design)
  (extrude 100
           (interpolation-surface
             '([(100 0 0) (1 0 0)]
               [(-100 0 0) (-1 0 0)]
               [(0 -100 0) (0 -1 0)]
               [(0 100 0) (0 1 0)]))))
