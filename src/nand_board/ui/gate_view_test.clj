(ns nand-board.ui.gate-view-test
  (:require [clojure.test :refer :all]
            [nand-board.logic.board :refer [add-gates
                                            last-added-gates
                                            make-initial-board]]
            [nand-board.ui.gate-view :refer [->GateView]]
            [nand-board.ui.view :refer [bounds
                                        overlaps?]]))

(def gates (-> (make-initial-board) (add-gates 2) last-added-gates))
(def g1 [gates 0])
(def g2 [gates 1])

(deftest about-gate-view

  (testing
    "should have the correct bounds"
    (is (= (bounds (->GateView g1 [200 300]))) [(/ 275 2) 273 125 54]))

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
      (is (no-overlaps [200 300] [200 (+ 300 55)])))))
