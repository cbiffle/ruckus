#lang ruckus

(rotate 45 #:around 'y
  (extrude 100
    (difference
      (circle 100)
      (slice
        (radial-repeat 3
          (at '[55 0 0] (sphere 30))))
      (rect 20 20))))
