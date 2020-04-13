(ns nand-board.logic.simulator-test
  (:require [midje.sweet :refer [=> =not=> fact]]
            [nand-board.logic.board :refer [add-gates
                                            add-wires
                                            last-added-gates
                                            make-initial-board
                                            pins-for-gates]]
            [nand-board.logic.simulator :refer :all]))

(defn- ticks [state]
  (if (pending-events? state)
    (lazy-seq (cons state (ticks (tick state))))
    [state]))

(defn- vals-over-time [state]
  (->> state ticks (map :vals)))

(fact
  "initial state should start at time 0"
  (let [board (-> (make-initial-board) (add-gates 1))
        state (-> (make-initial-state board))]
    (:time state) => 0))

(fact
  "tick should increase time by 1"
  (let [board (-> (make-initial-board))
        times (->> (make-initial-state board) (iterate tick) (map :time))]
    (take 3 times) => [0 1 2]))

(fact
  "initial state should have input pins set to 0"
  (let [board (-> (make-initial-board) (add-gates 2))
        vals-over-time (-> (make-initial-state board) vals-over-time)]
    (first vals-over-time) => {0 0, 1 0, 3 0, 4 0}))

(fact
  "initial state should have only unwired input pins set to 0"
  (let [board1 (-> (make-initial-board) (add-gates 2))
        [_ _ o3 i4 _ _] (pins-for-gates board1 (last-added-gates board1))
        board2 (-> board1 (add-wires [o3 i4]))
        vals-over-time (-> (make-initial-state board2) vals-over-time)]
    (first vals-over-time) => {0 0, 1 0, 4 0}))

(fact
  "propagation delay from input pin to output pin should be 2"
  (let [board (-> (make-initial-board) (add-gates 2))
        vals-over-time (-> (make-initial-state board) vals-over-time)]
    vals-over-time => [{0 0, 1 0, 3 0, 4 0}
                       {0 0, 1 0, 3 0, 4 0}
                       {0 0, 1 0, 2 1, 3 0, 4 0, 5 1}]))

(fact
  "propagation delay through a wire should be 1"
  (let [board1 (-> (make-initial-board) (add-gates 2))
        [_ _ o3 i4 _ _] (pins-for-gates board1 (last-added-gates board1))
        board2 (-> board1 (add-wires [o3 i4]))
        vals-over-time (-> (make-initial-state board2) vals-over-time)]
    vals-over-time => [{0 0, 1 0, 4 0}
                       {0 0, 1 0, 4 0}
                       {0 0, 1 0, 2 1, 4 0}
                       {0 0, 1 0, 2 1, 3 1, 4 0}
                       {0 0, 1 0, 2 1, 3 1, 4 0}
                       {0 0, 1 0, 2 1, 3 1, 4 0, 5 1}]))

(fact
  "gate should be a NAND"
  (let [board (-> (make-initial-board) (add-gates 1))
        [i1 i2 _] (pins-for-gates board (last-added-gates board))
        state00 (-> (make-initial-state board) ticks last)
        state10 (-> state00 (set-val i1 1) ticks last)
        state11 (-> state10 (set-val i2 1) ticks last)
        state01 (-> state11 (set-val i1 0) ticks last)]
    (:vals state00) => {0 0, 1 0, 2 1}
    (:vals state10) => {0 1, 1 0, 2 1}
    (:vals state11) => {0 1, 1 1, 2 0}
    (:vals state01) => {0 0, 1 1, 2 1}))

(fact
  "changes to both input pins of gate at the same time should propagate as if they were set at the same time"
  (let [board (-> (make-initial-board) (add-gates 1))
        [i1 i2 _] (pins-for-gates board (last-added-gates board))
        state1 (-> (make-initial-state board) ticks last)
        state2 (-> state1 (set-val i1 1) (set-val i2 1) ticks last)]
    (:vals state1) => {0 0, 1 0, 2 1}
    (:vals state2) => {0 1, 1 1, 2 0}))

(fact
  "changes to input pins while a propagation is already in process should propagate correctly"
  (let [board (-> (make-initial-board) (add-gates 1))
        [i1 i2 _] (pins-for-gates board (last-added-gates board))
        vals-over-time (-> (make-initial-state board) tick (set-val i1 1) (set-val i2 1) vals-over-time)]
    vals-over-time => [{0 1, 1 1}
                       {0 1, 1 1, 2 1}
                       {0 1, 1 1, 2 0}]))

(fact
  "events on input pin should only propagate if it is a change"
  (let [board (-> (make-initial-board) (add-gates 1))
        [i1 _ _] (pins-for-gates board (last-added-gates board))
        state (-> (make-initial-state board) ticks last)]
    (-> state (set-val i1 0)) =not=> pending-events?
    (-> state (set-val i1 1)) => pending-events?
    (-> state (set-val i1 1) ticks last (set-val i1 1)) =not=> pending-events?))

(fact
  "events on output pin should only propagate if it is a change"
  (let [board1 (-> (make-initial-board) (add-gates 2))
        [i1 i2 o3 i4 _ _] (pins-for-gates board1 (last-added-gates board1))
        board2 (-> board1 (add-wires [o3 i4]))
        state (-> (make-initial-state board2) ticks last)]
    (-> state (set-val i1 1) tick tick) =not=> pending-events?
    (-> state (set-val i1 1) (set-val i2 1) tick tick) => pending-events?))

(fact
  "output pin can be wired to more than one input pin"
  (let [board1 (-> (make-initial-board) (add-gates 2))
        [i1 i2 o3 i4 i5 _] (pins-for-gates board1 (last-added-gates board1))
        board2 (-> board1 (add-wires [o3 i4] [o3 i5]))
        state1 (-> (make-initial-state board2) ticks last)
        state2 (-> state1 (set-val i1 1) (set-val i2 1) ticks last)]
    (:vals state1) => {0 0, 1 0, 2 1, 3 1, 4 1, 5 0}
    (:vals state2) => {0 1, 1 1, 2 0, 3 0, 4 0, 5 1}))
