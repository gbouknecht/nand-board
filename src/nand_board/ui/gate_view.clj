(ns nand-board.ui.gate-view
  (:require [nand-board.logic.board :refer [pins-for-gates
                                            unwired?]]
            [nand-board.logic.simulator :refer [get-val
                                                flip-val]]
            [nand-board.ui.view :refer [bounds
                                        View]]
            [nand-board.ui.ui-helpers :refer [translate-geometry]]
            [quil.core :as q]))

(def ^:private size 50)
(def ^:private half-size (/ size 2))
(def ^:private quarter-size (/ size 4))
(def ^:private wire-length (* quarter-size 3))
(def ^:private inversion-diameter 15)
(def ^:private weight 4)

(defn geometry [center]
  "
              0 - - - - - 1
        12    |             ++
    5 - - - - 6                ++
              |                  ++   OOOO    14
              |           2        9 O 11 O - - - 10
        13    |                  ++   OOOO
    7 - - - - 8                ++
              |             ++
              4 - - - - - 3
  "
  (let [p0 [(- half-size) (- half-size)]
        p1 [0 (p0 1)]
        p2 [(p1 0) 0]
        p3 [(p1 0) half-size]
        p4 [(p0 0) (p3 1)]
        p5 [(- (p0 0) wire-length) (- quarter-size)]
        p6 [(p0 0) (p5 1)]
        p7 [(p5 0) quarter-size]
        p8 [(p0 0) (p7 1)]
        p9 [half-size 0]
        p10 [(+ (p9 0) wire-length) (p9 1)]
        p11 [(+ (p9 0) (/ inversion-diameter 2)) (p9 1)]
        p12 [(+ (p5 0) quarter-size) (- (p5 1) (/ quarter-size 2))]
        p13 [(+ (p7 0) quarter-size) (- (p7 1) (/ quarter-size 2))]
        p14 [(- (p10 0) quarter-size) (- (p10 1) (/ quarter-size 2))]]
    (translate-geometry
      {:p0  p0
       :p1  p1
       :p2  p2
       :p3  p3
       :p4  p4
       :p5  p5
       :p6  p6
       :p7  p7
       :p8  p8
       :p9  p9
       :p10 p10
       :p11 p11
       :p12 p12
       :p13 p13
       :p14 p14}
      center)))

(defn- x0-y0-x1-y1 [bounds]
  (let [[x0 y0 width height] bounds
        [x1 y1] [(+ x0 width) (+ y0 height)]]
    [x0 y0 x1 y1]))

(defrecord GateView [gate center]
  View
  (draw [_ state]
    (let [[i1 i2 o3] (pins-for-gates (:board state) [gate])
          geometry (geometry center)
          [x2 y2] (:p2 geometry)
          [x11 y11] (:p11 geometry)
          [x12 y12] (:p12 geometry)
          [x13 y13] (:p13 geometry)
          [x14 y14] (:p14 geometry)]
      (q/push-style)
      (q/stroke-weight weight)
      (q/line (:p0 geometry) (:p1 geometry))
      (q/arc x2 y2 size size (- q/HALF-PI) q/HALF-PI)
      (q/line (:p3 geometry) (:p4 geometry))
      (q/line (:p4 geometry) (:p0 geometry))
      (q/stroke-weight (/ weight 2))
      (q/line (:p5 geometry) (:p6 geometry))
      (q/line (:p7 geometry) (:p8 geometry))
      (q/line (:p9 geometry) (:p10 geometry))
      (q/stroke-weight weight)
      (q/ellipse x11 y11 inversion-diameter inversion-diameter)
      (q/fill 0)
      (q/text (str (get-val state i1)) x12 y12)
      (q/text (str (get-val state i2)) x13 y13)
      (q/text (str (get-val state o3)) x14 y14)
      (q/pop-style)))
  (bounds [_]
    (let [[x y] center
          left (- x half-size wire-length)
          top (- y half-size (/ weight 2))
          width (+ wire-length size wire-length)
          height (+ size weight)]
      [left top width height]))
  (contains-coords? [this [x y]]
    (let [[x0 y0 x1 y1] (x0-y0-x1-y1 (bounds this))]
      (and (<= x0 x x1)
           (<= y0 y y1))))
  (overlaps? [this that]
    (let [[this-x0 this-y0 this-x1 this-y1] (x0-y0-x1-y1 (bounds this))
          [that-x0 that-y0 that-x1 that-y1] (x0-y0-x1-y1 (bounds that))]
      (and (or (<= that-x0 this-x0 that-x1)
               (<= that-x0 this-x1 that-x1))
           (or (<= that-y0 this-y0 that-y1)
               (<= that-y0 this-y1 that-y1)))))
  (process-single-click [_ [x y] state]
    (let [board (:board state)
          [i1 i2 _] (pins-for-gates board [gate])
          geometry (geometry center)
          [_ y0] (:p0 geometry)
          [_ y2] (:p2 geometry)
          [x5 y5] (:p5 geometry)
          [x6 _] (:p6 geometry)
          [x7 y7] (:p7 geometry)
          [x8 _] (:p8 geometry)]
      (cond
        (and (<= x5 x x6) (<= y0 y y5) (unwired? board i1)) (flip-val state i1)
        (and (<= x7 x x8) (<= y2 y y7) (unwired? board i2)) (flip-val state i2)
        :else state))))
