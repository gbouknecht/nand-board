(ns nand-board.ui.gate-view-test
  (:require [clojure.test :refer :all]
            [nand-board.logic.board :refer [add-gates
                                            add-wires
                                            input-pins-for-gate
                                            last-added-gates
                                            make-initial-board
                                            output-pin-for-gate]]
            [nand-board.logic.simulator :refer [flip-val
                                                make-initial-state
                                                ticks]]
            [nand-board.ui.gate-view :refer :all]
            [nand-board.ui.ui-helpers :refer [translate-geometry]]
            [nand-board.ui.view :refer [bounds
                                        contains-coords?
                                        overlaps?
                                        process-single-click]]))

(def board (-> (make-initial-board) (add-gates 2)))
(def gates (last-added-gates board))
(def g1 (gates 0))
(def g2 (gates 1))

(deftest about-gate-view

  (testing
    "should have the correct bounds"
    (is (= (bounds (->GateView g1 [200 300]))) [(/ 275 2) 273 125 54]))

  (testing
    "should be able to say if it contains specific coordinates"
    (let [gv1 (->GateView g1 [200 300])
          [x0 y0 width height] (bounds gv1)
          [x1 y1] [(+ x0 width) (+ y0 height)]]
      (is (contains-coords? gv1 [200 300]))
      (is (contains-coords? gv1 [x0 y0]))
      (is (not (contains-coords? gv1 [(dec x0) y0])))
      (is (not (contains-coords? gv1 [x0 (dec y0)])))
      (is (contains-coords? gv1 [x1 y0]))
      (is (not (contains-coords? gv1 [(inc x1) y0])))
      (is (not (contains-coords? gv1 [x1 (dec y0)])))
      (is (contains-coords? gv1 [x0 y1]))
      (is (not (contains-coords? gv1 [(dec x0) y1])))
      (is (not (contains-coords? gv1 [x0 (inc y1)])))
      (is (contains-coords? gv1 [x1 y1]))
      (is (not (contains-coords? gv1 [(inc x1) y1])))
      (is (not (contains-coords? gv1 [x1 (inc y1)])))))

  (testing
    "should be able to detect overlapping view"
    (letfn [(verify-overlaps? [center1 center2 expected]
              (= (overlaps? (->GateView g1 center1) (->GateView g2 center2)) expected))
            (overlaps [center1 center2]
              (and (verify-overlaps? center1 center2 true)
                   (verify-overlaps? center2 center1 true)))
            (no-overlaps [center1 center2]
              (and (verify-overlaps? center1 center2 false)
                   (verify-overlaps? center2 center1 false)))]
      (is (overlaps [200 300] [200 300]))
      (is (overlaps [200 300] [(- 200 124) 300]))
      (is (overlaps [200 300] [(- 200 125) 300]))
      (is (no-overlaps [200 300] [(- 200 126) 300]))
      (is (overlaps [200 300] [(+ 200 124) 300]))
      (is (overlaps [200 300] [(+ 200 125) 300]))
      (is (no-overlaps [200 300] [(+ 200 126) 300]))
      (is (overlaps [200 300] [200 (- 300 53)]))
      (is (overlaps [200 300] [200 (- 300 54)]))
      (is (no-overlaps [200 300] [200 (- 300 55)]))
      (is (overlaps [200 300] [200 (+ 300 53)]))
      (is (overlaps [200 300] [200 (+ 300 54)]))
      (is (no-overlaps [200 300] [200 (+ 300 55)]))))

  (testing
    "single click processing"

    (let [state (make-initial-state board)
          center [200 300]
          gv1 (->GateView g1 center)
          [i1 i2] (input-pins-for-gate board g1)
          geometry (geometry center)
          [_ y0] (:p0 geometry)
          [_ y2] (:p2 geometry)
          [x5 _ :as p5] (:p5 geometry)
          p6 (:p6 geometry)
          [x7 _ :as p7] (:p7 geometry)
          p8 (:p8 geometry)
          p14 (:p14 geometry)]

      (testing
        "should not change state if no input pin is clicked"
        (is (= (process-single-click gv1 center state) state))
        (is (= (process-single-click gv1 p14 state) state))
        (is (= (process-single-click gv1 [400 500] state) state)))

      (testing
        "should flip value if input pin is clicked"
        (is (= (process-single-click gv1 p5 state) (flip-val state i1)))
        (is (= (process-single-click gv1 p6 state) (flip-val state i1)))
        (is (= (process-single-click gv1 [x5 y0] state) (flip-val state i1)))
        (is (= (process-single-click gv1 p7 state) (flip-val state i2)))
        (is (= (process-single-click gv1 p8 state) (flip-val state i2)))
        (is (= (process-single-click gv1 [x7 y2] state) (flip-val state i2))))

      (testing
        "should not change state if input pin is wired"
        (let [o6 (output-pin-for-gate board g2)]
          (let [state (-> (make-initial-state (add-wires board [o6 i1])) ticks last)]
            (is (= (process-single-click gv1 p5 state) state))
            (is (= (process-single-click gv1 p7 state) (flip-val state i2))))
          (let [state (-> (make-initial-state (add-wires board [o6 i2])) ticks last)]
            (is (= (process-single-click gv1 p5 state) (flip-val state i1)))
            (is (= (process-single-click gv1 p7 state) state))))))))
