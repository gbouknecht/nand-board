(ns nand-board.ui.gate-view-test
  (:require [midje.sweet :refer [=> fact]]
            [nand-board.logic.board :refer [add-gates
                                            last-added-gates
                                            make-initial-board]]
            [nand-board.ui.gate-view :refer [->GateView]]
            [nand-board.ui.view :refer [bounds]]))

(def gates (-> (make-initial-board) (add-gates 1) last-added-gates))

(fact
  "should have the correct bounds"
  (let [[g1] gates]
    (bounds (->GateView g1 [200 300])) => [(/ 275 2) 273 125 54]))
