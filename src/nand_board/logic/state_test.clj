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

  (let [gate {:gate-id 0 :inputs [{:pin-id 11 :val 0} {:pin-id 13 :val 1}] :output {:pin-id 15 :val 1}}]
    (s/valid? ::state/gate gate) => true
    (s/valid? ::state/gate (assoc-in gate [:inputs 1 :pin-id] 11)) => false
    (s/valid? ::state/gate (assoc-in gate [:output :pin-id] 11)) => false
    (s/valid? ::state/gate (assoc-in gate [:output :pin-id] 13)) => false
    (let [gates {0 gate 1 (assoc gate :gate-id 1)}]
      (s/valid? ::state/gates gates) => true
      (s/valid? ::state/gates (assoc-in gates [0 :gate-id] 1)) => false
      (s/valid? ::state/gates (assoc-in gates [0 :gate-id] 2)) => false))

  (s/valid? ::state/wire-ids #{}) => false
  (s/valid? ::state/wire-ids #{0}) => true
  (s/valid? ::state/wire-ids #{0 1}) => true
  (s/valid? ::state/wire-ids #{0 1 2}) => true
  (s/valid? ::state/wire-ids #{0 -1 2}) => false
  (s/valid? ::state/wire-ids #{0 "1" 2}) => false

  (let [pin {:pin-id 0 :gate-id 0}
        pins {0 pin 1 (assoc pin :pin-id 1)}]
    (s/valid? ::state/pins pins) => true
    (s/valid? ::state/pins (assoc-in pins [0 :pin-id] 1)) => false
    (s/valid? ::state/pins (assoc-in pins [0 :pin-id] 2)) => false)

  (let [wire {:wire-id 0 :output-pin-id 0 :input-pin-id 1}]
    (s/valid? ::state/wire wire) => true
    (s/valid? ::state/wire (assoc wire :input-pin-id 0)) => false
    (let [wires {0 wire 1 (assoc wire :wire-id 1)}]
      (s/valid? ::state/wires wires) => true
      (s/valid? ::state/wires (assoc-in wires [0 :wire-id] 1)) => false
      (s/valid? ::state/wires (assoc-in wires [0 :wire-id] 2)) => false))

  (let [state {:gates {0 {:gate-id 0 :inputs [{:pin-id 0 :val 0} {:pin-id 1 :val 0}] :output {:pin-id 2 :val 1}}
                       1 {:gate-id 1 :inputs [{:pin-id 3 :val 0} {:pin-id 4 :val 1}] :output {:pin-id 5 :val 1}}
                       2 {:gate-id 2 :inputs [{:pin-id 6 :val 1} {:pin-id 7 :val 1}] :output {:pin-id 8 :val 0}}}
               :pins  {0 {:pin-id 0 :gate-id 0}
                       1 {:pin-id 1 :gate-id 0}
                       2 {:pin-id 2 :gate-id 0 :wire-ids #{0 1}}
                       3 {:pin-id 3 :gate-id 1}
                       4 {:pin-id 4 :gate-id 1 :wire-ids #{0}}
                       5 {:pin-id 5 :gate-id 1 :wire-ids #{2}}
                       6 {:pin-id 6 :gate-id 2 :wire-ids #{1}}
                       7 {:pin-id 7 :gate-id 2 :wire-ids #{2}}
                       8 {:pin-id 8 :gate-id 2}}
               :wires {0 {:wire-id 0 :output-pin-id 2 :input-pin-id 4}
                       1 {:wire-id 1 :output-pin-id 2 :input-pin-id 6}
                       2 {:wire-id 2 :output-pin-id 5 :input-pin-id 7}}}]
    (s/valid? ::state/state {:gates {} :pins {} :wires {}}) => true
    (s/valid? ::state/state state) => true
    (s/valid? ::state/state (assoc-in state [:pins 1 :gate-id] 1)) => false
    (s/valid? ::state/state (assoc-in state [:pins 2 :gate-id] 1)) => false
    (s/valid? ::state/state (assoc-in state [:pins 1 :wire-ids] #{Integer/MAX_VALUE})) => false
    (s/valid? ::state/state (assoc-in state [:wires 1 :output-pin-id] 5)) => false
    (s/valid? ::state/state (assoc-in state [:wires 1 :input-pin-id] 0)) => false
    (s/valid? ::state/state (-> state
                                (assoc-in [:wires 3] {:wire-id 3 :output-pin-id 5 :input-pin-id 4})
                                (update-in [:pins 5 :wire-ids] conj 3)
                                (update-in [:pins 4 :wire-ids] conj 3))) => false
    (s/valid? ::state/state (-> state
                                (assoc-in [:wires 3] {:wire-id 3 :output-pin-id 0 :input-pin-id 1})
                                (assoc-in [:pins 0 :wire-ids] #{3})
                                (assoc-in [:pins 1 :wire-ids] #{3}))) => false
    (s/valid? ::state/state (-> state
                                (assoc-in [:wires 3] {:wire-id 3 :output-pin-id 2 :input-pin-id 5})
                                (update-in [:pins 2 :wire-ids] conj 3)
                                (update-in [:pins 5 :wire-ids] conj 3))) => false))

(fact
  "state/initialize should give a state with no gates, pins and wires"
  (let [state (state/initialize)]
    (:gates state) => empty?
    (:pins state) => empty?
    (:wires state) => empty?))

(fact
  "state/add-gate should add a new gate and pins, no wires"
  (let [state1 (state/add-gate (state/initialize))
        state2 (state/add-gate state1)]
    ((:gates state1) 0) => {:gate-id 0 :inputs [{:pin-id 0 :val 0} {:pin-id 1 :val 0}] :output {:pin-id 2 :val 0}}
    (:pins state1) => {0 {:pin-id 0 :gate-id 0}
                       1 {:pin-id 1 :gate-id 0}
                       2 {:pin-id 2 :gate-id 0}}
    (:wires state1) => empty?
    ((:gates state2) 0) => ((:gates state1) 0)
    ((:gates state2) 1) => {:gate-id 1 :inputs [{:pin-id 3 :val 0} {:pin-id 4 :val 0}] :output {:pin-id 5 :val 0}}
    (:pins state2) => (merge (:pins state1)
                             {3 {:pin-id 3 :gate-id 1}
                              4 {:pin-id 4 :gate-id 1}
                              5 {:pin-id 5 :gate-id 1}})
    (:wires state2) => empty?))
