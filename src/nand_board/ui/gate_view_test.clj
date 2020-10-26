(ns nand-board.ui.gate-view-test
  (:require [midje.sweet :refer [=> fact tabular]]
            [nand-board.logic.board :refer [add-gates
                                            last-added-gates
                                            make-initial-board]]
            [nand-board.ui.gate-view :refer [->GateView]]
            [nand-board.ui.view :refer [bounds
                                        overlaps?]]))

(def gates (-> (make-initial-board) (add-gates 2) last-added-gates))
(def g1 [gates 0])
(def g2 [gates 1])

(fact
  "should have the correct bounds"
  (bounds (->GateView g1 [200 300])) => [(/ 275 2) 273 125 54])

(declare ?center1 ?center2 ?expected)

(tabular
  (fact
    "should be able to detect overlapping view"
    [(overlaps? (->GateView g1 ?center1) (->GateView g2 ?center2))
     (overlaps? (->GateView g1 ?center2) (->GateView g2 ?center1))] => [?expected ?expected])
  [?center1 ?center2 ?expected]
  [200 300] [200 300] true
  [200 300] [(- 200 124) 300] true
  [200 300] [(- 200 125) 300] true
  [200 300] [(- 200 126) 300] false
  [200 300] [(+ 200 124) 300] true
  [200 300] [(+ 200 125) 300] true
  [200 300] [(+ 200 126) 300] false
  [200 300] [200 (- 300 53)] true
  [200 300] [200 (- 300 54)] true
  [200 300] [200 (- 300 55)] false
  [200 300] [200 (+ 300 53)] true
  [200 300] [200 (+ 300 54)] true
  [200 300] [200 (+ 300 55)] false)
