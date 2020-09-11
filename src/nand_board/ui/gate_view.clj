(ns nand-board.ui.gate-view
  (:require [nand-board.ui.drawable :refer [Drawable]]
            [quil.core :as q]))

(defrecord GateView [center]
  Drawable
  (draw [_]
    (let [size 50
          half-size (/ size 2)
          quarter-size (/ size 4)
          [x0 y0 :as p0] [(- half-size) (- half-size)]
          [x1 _ :as p1] [0 y0]
          [x2 y2] [x1 0]
          [_ y3 :as p3] [x1 half-size]
          [_ _ :as p4] [x0 y3]
          wire-length (* quarter-size 3)
          [x5 y5 :as p5] [(- x0 wire-length) (- quarter-size)]
          [_ _ :as p6] [x0 y5]
          [_ y7 :as p7] [x5 quarter-size]
          [_ _ :as p8] [x0 y7]
          [x9 y9 :as p9] [half-size 0]
          [_ _ :as p10] [(+ x9 wire-length) y9]
          inversion-diameter 15
          [x11 y11] [(+ x9 (/ inversion-diameter 2)) y9]]
      (q/push-matrix)
      (q/push-style)
      (q/translate center)
      (q/stroke-weight 4)
      (q/line p0 p1)
      (q/arc x2 y2 size size (- q/HALF-PI) q/HALF-PI)
      (q/line p3 p4)
      (q/line p4 p0)
      (q/stroke-weight 2)
      (q/line p5 p6)
      (q/line p7 p8)
      (q/line p9 p10)
      (q/stroke-weight 4)
      (q/ellipse x11 y11 inversion-diameter inversion-diameter)
      (q/pop-style)
      (q/pop-matrix))))
