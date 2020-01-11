(ns nand-board.logic.state-test
  (:require [midje.sweet :refer [=> =not=> contains fact throws]]
            [nand-board.logic.state :refer [add-gate
                                            add-wire
                                            make-initial-state
                                            remove-gate
                                            remove-wire
                                            remove-wires
                                            get-val
                                            set-val]]))

(fact
  "make-initial-state should give a state with no gates, pins and wires"
  (let [state (-> (make-initial-state))]
    (:gates state) => empty?
    (:pins state) => empty?
    (:wires state) => empty?))

(fact
  "add-gate should add a new gate and pins, no wires"
  (let [state1 (-> (make-initial-state) add-gate)
        state2 (-> state1 add-gate)]
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
  "add-wire should add a wire between two pins"
  (let [state1 (-> (make-initial-state) add-gate add-gate add-gate)
        state2 (-> state1 (add-wire 2 4))
        state3 (-> state2 (add-wire 2 6))]
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
  "add-wire should only accept an 'output' pin-id and an 'input' pin-id respectively"
  (let [state (-> (make-initial-state) add-gate add-gate)]
    (add-wire state 1 0) => (throws AssertionError)
    (add-wire state 8 0) => (throws AssertionError)
    (add-wire state 2 5) => (throws AssertionError)
    (add-wire state 2 6) => (throws AssertionError)))

(fact
  "remove-gate should remove gate, pins and wires"
  (let [state1 (-> (make-initial-state) add-gate add-gate add-gate)
        state2 (-> state1 (add-wire 2 4) (add-wire 2 6))
        state3 (-> state2 (remove-gate 1))]
    (dissoc (:gates state3) 1) => (dissoc (:gates state1) 1)
    (dissoc (:pins state3) 2 3 4 5) => (dissoc (:pins state2) 2 3 4 5)
    (dissoc (:wires state3) 0) => (dissoc (:wires state2) 0)
    (set (keys (:gates state3))) => #{0 2}
    (set (keys (:pins state3))) => #{0 1 2 6 7 8}
    (set (keys (:wires state3))) => #{1}
    ((:pins state3) 2) => {:pin-id 2 :gate-id 0 :wire-ids #{1}}
    ((:pins state3) 6) => {:pin-id 6 :gate-id 2 :wire-ids #{1}}))

(fact
  "remove-gate should be able to remove gate for which output is wired to own input"
  (let [state (-> (make-initial-state) add-gate (add-wire 2 0) (remove-gate 0))]
    (:gates state) => empty?))

(fact
  "remove-gate should be able to remove unwired gate"
  (let [state (-> (make-initial-state) add-gate (remove-gate 0))]
    (:gates state) => empty?))

(fact
  "remove-gate may only be called for an existing gate"
  (-> (make-initial-state) add-gate (remove-gate 1)) => (throws AssertionError))

(fact
  "remove-wire should remove wire"
  (let [state1 (-> (make-initial-state) add-gate add-gate add-gate)
        state2 (-> state1 (add-wire 2 4) (add-wire 2 3) (add-wire 2 6))
        state3 (-> state2 (remove-wire 0))]
    (:gates state3) => (:gates state1)
    (dissoc (:pins state3) 2 4) => (dissoc (:pins state2) 2 4)
    (set (keys (:wires state3))) => #{1 2}
    ((:pins state3) 2) => {:pin-id 2 :gate-id 0 :wire-ids #{1 2}}
    (keys ((:pins state3) 4)) =not=> (contains :wire-ids)))

(fact
  "remove-wire may only be called for an existing wire"
  (-> (make-initial-state) (remove-wire 0)) => (throws AssertionError))

(fact
  "remove-wires should remove wires"
  (let [state1 (-> (make-initial-state) add-gate add-gate add-gate)
        state2 (-> state1 (add-wire 2 4) (add-wire 2 3) (add-wire 2 6))
        state3 (-> state2 (remove-wires [0 2]))]
    (:gates state3) => (:gates state1)
    (dissoc (:pins state3) 2 4 6) => (dissoc (:pins state2) 2 4 6)
    (set (keys (:wires state3))) => #{1}
    ((:pins state3) 2) => {:pin-id 2 :gate-id 0 :wire-ids #{1}}
    (keys ((:pins state3) 4)) =not=> (contains :wire-ids)
    (keys ((:pins state3) 6)) =not=> (contains :wire-ids)))

(fact
  "remove-wires should do nothing if given wire-ids collection is empty"
  (let [state1 (-> (make-initial-state) add-gate (add-wire 2 0))
        state2 (-> state1 (remove-wires []))]
    state2 => state1))

(fact
  "remove-wires may only be called for existing and distinct wires"
  (let [state (-> (make-initial-state) add-gate (add-wire 2 0))]
    (remove-wires state [0 1]) => (throws AssertionError)
    (remove-wires state [0 0]) => (throws AssertionError)))

(fact
  "get-val and set-val should get/set value from/to gate inputs/output"
  (let [state1 (-> (make-initial-state) add-gate add-gate (add-wire 2 3))
        state2 (-> state1 (set-val 0 1))
        state3 (-> state2 (set-val 0 0))
        state4 (-> state3 (set-val 1 1) (set-val 2 1) (set-val 4 1))
        get-vals (fn [state] (map #(get-val state %) (range 0 6)))]
    (get-vals state2) => [1 0 0 0 0 0]
    (get-vals state3) => [0 0 0 0 0 0]
    (get-vals state4) => [0 1 1 0 1 0]))

(fact
  "get-val may only be called for an existing pin"
  (-> (make-initial-state) add-gate (get-val 3)) => (throws AssertionError))

(fact
  "set-val may only be called for an existing pin"
  (-> (make-initial-state) add-gate (set-val 3 1)) => (throws AssertionError))

(fact
  "set-val may only be called for a valid value"
  (let [state (-> (make-initial-state) add-gate)]
    (set-val state 0 -1) => (throws AssertionError)
    (set-val state 0 0) =not=> (throws AssertionError)
    (set-val state 0 1) =not=> (throws AssertionError)
    (set-val state 0 2) => (throws AssertionError)))
