(ns nand-board.logic.state-test
  (:require [clojure.spec.alpha :as s]
            [nand-board.logic.state :as state]
            [midje.sweet :refer [=> fact facts]]))

(facts
  "state data structure"

  (s/valid? ::state/id -1) => false
  (s/valid? ::state/id 0) => true
  (s/valid? ::state/id 1) => true
  (s/valid? ::state/id 2) => true
  (s/valid? ::state/id "2") => false

  (s/valid? ::state/val -1) => false
  (s/valid? ::state/val 0) => true
  (s/valid? ::state/val 1) => true
  (s/valid? ::state/val 2) => false

  (s/valid? ::state/inputs []) => false
  (s/valid? ::state/inputs [{:pin-id 11 :val 0}]) => false
  (s/valid? ::state/inputs [{:pin-id 11 :val 0} {:pin-id 13 :val 0}]) => true
  (s/valid? ::state/inputs [{:pin-id 11 :val 0} {:pin-id 13 :val 1} {:pin-id 15 :val 0}]) => false

  (s/valid? ::state/gate {:inputs [{:pin-id 11 :val 0} {:pin-id 11 :val 1}] :output {:pin-id 15 :val 1}}) => false
  (s/valid? ::state/gate {:inputs [{:pin-id 11 :val 0} {:pin-id 13 :val 1}] :output {:pin-id 11 :val 1}}) => false
  (s/valid? ::state/gate {:inputs [{:pin-id 11 :val 0} {:pin-id 13 :val 1}] :output {:pin-id 13 :val 1}}) => false
  (s/valid? ::state/gate {:inputs [{:pin-id 11 :val 0} {:pin-id 13 :val 1}] :output {:pin-id 15 :val 1}}) => true

  (s/valid? ::state/wire-ids #{}) => false
  (s/valid? ::state/wire-ids #{0}) => true
  (s/valid? ::state/wire-ids #{0 1}) => true
  (s/valid? ::state/wire-ids #{0 1 2}) => true
  (s/valid? ::state/wire-ids #{0 -1 2}) => false
  (s/valid? ::state/wire-ids #{0 "1" 2}) => false

  (s/valid? ::state/wire {:output-pin-id 0 :input-pin-id 0}) => false
  (s/valid? ::state/wire {:output-pin-id 0 :input-pin-id 1}) => true

  (let [state {:gates {0 {:inputs [{:pin-id 0 :val 0} {:pin-id 1 :val 0}] :output {:pin-id 2 :val 1}}
                       1 {:inputs [{:pin-id 3 :val 0} {:pin-id 4 :val 1}] :output {:pin-id 5 :val 1}}
                       2 {:inputs [{:pin-id 6 :val 1} {:pin-id 7 :val 1}] :output {:pin-id 8 :val 0}}}
               :pins  {0 {:gate-id 0}
                       1 {:gate-id 0}
                       2 {:gate-id 0 :wire-ids #{0 1}}
                       3 {:gate-id 1}
                       4 {:gate-id 1 :wire-ids #{0}}
                       5 {:gate-id 1 :wire-ids #{2}}
                       6 {:gate-id 2 :wire-ids #{1}}
                       7 {:gate-id 2 :wire-ids #{2}}
                       8 {:gate-id 2}}
               :wires {0 {:output-pin-id 2 :input-pin-id 4}
                       1 {:output-pin-id 2 :input-pin-id 6}
                       2 {:output-pin-id 5 :input-pin-id 7}}}]
    (s/valid? ::state/state {:gates {} :pins {} :wires {}}) => true
    (s/valid? ::state/state state) => true
    (s/valid? ::state/state (update-in state [:pins 1] assoc :gate-id 1)) => false
    (s/valid? ::state/state (update-in state [:pins 2] assoc :gate-id 1)) => false
    (s/valid? ::state/state (update-in state [:pins 1] assoc :wire-ids #{Integer/MAX_VALUE})) => false
    (s/valid? ::state/state (update-in state [:wires 1] assoc :output-pin-id 5)) => false
    (s/valid? ::state/state (update-in state [:wires 1] assoc :input-pin-id 0)) => false
    (s/valid? ::state/state (-> state
                                (update-in [:wires] assoc 3 {:output-pin-id 5 :input-pin-id 4})
                                (update-in [:pins 5 :wire-ids] conj 3)
                                (update-in [:pins 4 :wire-ids] conj 3))) => false
    (s/valid? ::state/state (-> state
                                (update-in [:wires] assoc 3 {:output-pin-id 0 :input-pin-id 1})
                                (update-in [:pins 0] assoc :wire-ids #{3})
                                (update-in [:pins 1] assoc :wire-ids #{3}))) => false
    (s/valid? ::state/state (-> state
                                (update-in [:wires] assoc 3 {:output-pin-id 2 :input-pin-id 5})
                                (update-in [:pins 2 :wire-ids] conj 3)
                                (update-in [:pins 5 :wire-ids] conj 3))) => false))

(fact
  "state initialization should give a state with no gates, pins and wires"
  (let [state (state/initialize)]
    (empty? (:gates state)) => true
    (empty? (:pins state)) => true
    (empty? (:wires state)) => true))
