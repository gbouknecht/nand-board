(ns nand-board.logic.simulator-test
  (:require [midje.sweet :refer [=> fact]]
            [nand-board.logic.board :refer [add-gate
                                            add-wire
                                            make-initial-board]]
            [nand-board.logic.simulator :refer [make-initial-state
                                                set-val
                                                tick]]))

(defn- ticks
  ([state] (iterate tick state))
  ([state n] (-> state ticks (nth n))))

(defn- vals-over-time [state]
  (->> state ticks (map :vals)))

(fact
  "initial state should start at time 0"
  (let [board (-> (make-initial-board) add-gate)
        state (-> (make-initial-state board))]
    (:time state) => 0))

(fact
  "tick should increase time by 1"
  (let [board (-> (make-initial-board))
        times (->> (make-initial-state board) ticks (map :time))]
    (take 3 times) => [0 1 2]))

(fact
  "initial state should have input pins set to 0"
  (let [board (-> (make-initial-board) add-gate add-gate)
        vals-over-time (-> (make-initial-state board) vals-over-time)]
    (first vals-over-time) => {0 0, 1 0, 3 0, 4 0}))

(fact
  "initial state should have only unwired input pins set to 0"
  (let [board (-> (make-initial-board) add-gate add-gate (add-wire 2 3))
        vals-over-time (-> (make-initial-state board) vals-over-time)]
    (first vals-over-time) => {0 0, 1 0, 4 0}))

(fact
  "propagation delay from input pin to output pin should be 2"
  (let [board (-> (make-initial-board) add-gate add-gate)
        vals-over-time (-> (make-initial-state board) vals-over-time)]
    (take 3 vals-over-time) => [{0 0, 1 0, 3 0, 4 0}
                                {0 0, 1 0, 3 0, 4 0}
                                {0 0, 1 0, 2 1, 3 0, 4 0, 5 1}]))

(fact
  "propagation delay through a wire should be 1"
  (let [board (-> (make-initial-board) add-gate add-gate (add-wire 2 3))
        vals-over-time (-> (make-initial-state board) vals-over-time)]
    (take 6 vals-over-time) => [{0 0, 1 0, 4 0}
                                {0 0, 1 0, 4 0}
                                {0 0, 1 0, 2 1, 4 0}
                                {0 0, 1 0, 2 1, 3 1, 4 0}
                                {0 0, 1 0, 2 1, 3 1, 4 0}
                                {0 0, 1 0, 2 1, 3 1, 4 0, 5 1}]))

(fact
  "gate should be a NAND"
  (let [board (-> (make-initial-board) add-gate)
        state00 (-> (make-initial-state board) (ticks 2))
        state10 (-> state00 (set-val 0 1) (ticks 2))
        state11 (-> state10 (set-val 1 1) (ticks 2))
        state01 (-> state11 (set-val 0 0) (ticks 2))]
    (:vals state00) => {0 0, 1 0, 2 1}
    (:vals state10) => {0 1, 1 0, 2 1}
    (:vals state11) => {0 1, 1 1, 2 0}
    (:vals state01) => {0 0, 1 1, 2 1}))

(fact
  "changes to both input pins of gate at the same time should propagate as if they were set at the same time"
  (let [board (-> (make-initial-board) add-gate)
        state2 (-> (make-initial-state board) (ticks 2))
        state5 (-> state2 (set-val 0 1) (set-val 1 1) (ticks 3))]
    (:vals state2) => {0 0, 1 0, 2 1}
    (:vals state5) => {0 1, 1 1, 2 0}))

(fact
  "output pin can be wired to more than one input pin"
  (let [board (-> (make-initial-board) add-gate add-gate (add-wire 2 3) (add-wire 2 4))
        state5 (-> (make-initial-state board) (ticks 5))
        state10 (-> state5 (set-val 0 1) (set-val 1 1) (ticks 5))]
    (:vals state5) => {0 0, 1 0, 2 1, 3 1, 4 1, 5 0}
    (:vals state10) => {0 1, 1 1, 2 0, 3 0, 4 0, 5 1}))