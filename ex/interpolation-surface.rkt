#lang ruckus

(extrude 100
  (slice
    (interpolation-surface
      '([(100 0 0) (1 0 0)]
        [(-100 0 0) (-1 0 0)]
        [(0 -100 0) (0 -1 0)]
        [(0 100 0) (0 1 0)]))))
