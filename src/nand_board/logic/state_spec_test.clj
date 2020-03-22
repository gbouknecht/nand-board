(ns nand-board.logic.state-spec-test
  (:require [clojure.spec.alpha :as s]
            [midje.sweet :refer [=> facts]]
            [nand-board.logic.board :refer [add-gates make-initial-board]]
            [nand-board.logic.state-spec :as state-spec]))

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
        state {:time 3
               :board board
               :vals {0 0, 2 1, 3 0, 4 0, 5 1}
               :event-queue #{{:time 3 :pin-id 2 :val 1}
                              {:time 4 :pin-id 0 :val 1}
                              {:time 4 :pin-id 1 :val 1}
                              {:time 5 :pin-id 4 :val 1}
                              {:time 9 :pin-id 2 :val 0}}}]
    (s/valid? ::state-spec/state {:time 0 :board (make-initial-board) :vals {} :event-queue #{}}) => true
    (s/valid? ::state-spec/state state) => true
    (s/valid? ::state-spec/state (assoc-in state [:vals] {6 0})) => false
    (s/valid? ::state-spec/state (update-in state [:event-queue] conj {:time 5 :pin-id 6 :val 1})) => false
    (s/valid? ::state-spec/state (update-in state [:event-queue] conj {:time 2 :pin-id 0 :val 1})) => false))
