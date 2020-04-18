(ns nand-board.logic.state-spec-test
  (:require [clojure.spec.alpha :as s]
            [midje.sweet :refer [=> facts]]
            [nand-board.logic.board :refer [add-gates
                                            last-added-gates
                                            make-initial-board
                                            pins-for-gates]]
            [nand-board.logic.state-spec :as state-spec]))

(defn- make-event-queue []
  (sorted-set-by #(compare [(:time %1) (:pin-id %1)] [(:time %2) (:pin-id %2)])))

(facts
  "state data structure"

  (s/valid? ::state-spec/time -1) => false
  (s/valid? ::state-spec/time 0) => true
  (s/valid? ::state-spec/time 0.5) => false
  (s/valid? ::state-spec/time 1) => true
  (s/valid? ::state-spec/time 2) => true
  (s/valid? ::state-spec/time "2") => false

  (s/valid? ::state-spec/val -1) => false
  (s/valid? ::state-spec/val 0) => true
  (s/valid? ::state-spec/val 1) => true
  (s/valid? ::state-spec/val 2) => false

  (let [board (-> (make-initial-board) (add-gates 2))
        [i1 i2 o3 i4 i5 o6] (pins-for-gates board (last-added-gates board))
        state {:time        3
               :board       board
               :vals        {(:id i1) 0, (:id o3) 1, (:id i4) 0, (:id i5) 0, (:id o6) 1}
               :event-queue (conj (make-event-queue)
                                  {:time 3 :pin-id (:id o3) :val 1}
                                  {:time 4 :pin-id (:id i1) :val 1}
                                  {:time 4 :pin-id (:id i2) :val 1}
                                  {:time 5 :pin-id (:id i5) :val 1}
                                  {:time 9 :pin-id (:id o3) :val 0})}]
    (s/valid? ::state-spec/state {:time 0 :board (make-initial-board) :vals {} :event-queue (make-event-queue)}) => true
    (s/valid? ::state-spec/state state) => true
    (s/valid? ::state-spec/state (assoc-in state [:vals] {Integer/MAX_VALUE 0})) => false
    (s/valid? ::state-spec/state (update-in state [:event-queue] conj {:time 5 :pin-id Integer/MAX_VALUE :val 1})) => false
    (s/valid? ::state-spec/state (update-in state [:event-queue] conj {:time 2 :pin-id (:id i1) :val 1})) => false))
