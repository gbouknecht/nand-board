(ns nand-board.logic.state-test
  (:require [midje.sweet :refer [=> =not=> contains fact throws]]
            [nand-board.logic.state :as state]))

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

(fact
  "state/add-wire should add a wire between two pins"
  (let [state1 (-> (state/initialize) state/add-gate state/add-gate state/add-gate)
        state2 (state/add-wire state1 2 4)
        state3 (state/add-wire state2 2 6)]
    (:gates state2) => (:gates state1)
    (dissoc (:pins state2) 2 4) => (dissoc (:pins state1) 2 4)
    ((:pins state2) 2) => {:pin-id 2 :gate-id 0 :wire-ids #{0}}
    ((:pins state2) 4) => {:pin-id 4 :gate-id 1 :wire-ids #{0}}
    ((:wires state2) 0) => {:wire-id 0 :output-pin-id 2 :input-pin-id 4}

    (:gates state3) => (:gates state1)
    (dissoc (:pins state3) 2 6) => (dissoc (:pins state2) 2 6)
    ((:pins state3) 2) => {:pin-id 2 :gate-id 0 :wire-ids #{0 1}}
    ((:pins state3) 6) => {:pin-id 6 :gate-id 2 :wire-ids #{1}}
    ((:wires state3) 1) => {:wire-id 1 :output-pin-id 2 :input-pin-id 6}))

(fact
  "state/add-wire should only accept an 'output' pin-id and an 'input' pin-id respectively"
  (let [state (-> (state/initialize) state/add-gate state/add-gate)]
    (state/add-wire state 1 0) => (throws AssertionError)
    (state/add-wire state 8 0) => (throws AssertionError)
    (state/add-wire state 2 5) => (throws AssertionError)
    (state/add-wire state 2 6) => (throws AssertionError)))

(fact
  "state/remove-gate should remove gate, pins and wires"
  (let [state1 (-> (state/initialize) state/add-gate state/add-gate state/add-gate)
        state2 (-> state1 (state/add-wire 2 4) (state/add-wire 2 6))
        state3 (state/remove-gate state2 1)]
    (dissoc (:gates state3) 1) => (dissoc (:gates state1) 1)
    (dissoc (:pins state3) 2 3 4 5) => (dissoc (:pins state2) 2 3 4 5)
    (dissoc (:wires state3) 0) => (dissoc (:wires state2) 0)
    (set (keys (:gates state3))) => #{0 2}
    (set (keys (:pins state3))) => #{0 1 2 6 7 8}
    (set (keys (:wires state3))) => #{1}
    ((:pins state3) 2) => {:pin-id 2 :gate-id 0 :wire-ids #{1}}
    ((:pins state3) 6) => {:pin-id 6 :gate-id 2 :wire-ids #{1}}))

(fact
  "state/remove-gate should be able to remove gate for which output is wired to own input"
  (let [state (-> (state/initialize) state/add-gate (state/add-wire 2 0) (state/remove-gate 0))]
    (:gates state) => empty?))

(fact
  "state/remove-gate should be able to remove unwired gate"
  (let [state (-> (state/initialize) state/add-gate (state/remove-gate 0))]
    (:gates state) => empty?))

(fact
  "state/remove-gate may only be called for an existing gate"
  (-> (state/initialize) state/add-gate (state/remove-gate 1)) => (throws AssertionError))

(fact
  "state/remove-wire should remove wire"
  (let [state1 (-> (state/initialize) state/add-gate state/add-gate state/add-gate)
        state2 (-> state1 (state/add-wire 2 4) (state/add-wire 2 3) (state/add-wire 2 6))
        state3 (state/remove-wire state2 0)]
    (:gates state3) => (:gates state1)
    (dissoc (:pins state3) 2 4) => (dissoc (:pins state2) 2 4)
    (set (keys (:wires state3))) => #{1 2}
    ((:pins state3) 2) => {:pin-id 2 :gate-id 0 :wire-ids #{1 2}}
    (keys ((:pins state3) 4)) =not=> (contains :wire-ids)))

(fact
  "state/remove-wire my only be called for an existing wire"
  (-> (state/initialize) (state/remove-wire 0)) => (throws AssertionError))

(fact
  "state/remove-wires should remove wires"
  (let [state1 (-> (state/initialize) state/add-gate state/add-gate state/add-gate)
        state2 (-> state1 (state/add-wire 2 4) (state/add-wire 2 3) (state/add-wire 2 6))
        state3 (state/remove-wires state2 [0 2])]
    (:gates state3) => (:gates state1)
    (dissoc (:pins state3) 2 4 6) => (dissoc (:pins state2) 2 4 6)
    (set (keys (:wires state3))) => #{1}
    ((:pins state3) 2) => {:pin-id 2 :gate-id 0 :wire-ids #{1}}
    (keys ((:pins state3) 4)) =not=> (contains :wire-ids)
    (keys ((:pins state3) 6)) =not=> (contains :wire-ids)))

(fact
  "state/remove-wires should do nothing if given wire-ids collection is empty"
  (let [state1 (-> (state/initialize) state/add-gate (state/add-wire 2 0))
        state2 (state/remove-wires state1 [])]
    state2 => state1))
