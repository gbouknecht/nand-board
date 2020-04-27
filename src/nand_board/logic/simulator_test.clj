(ns nand-board.logic.simulator-test
  (:require [midje.sweet :refer [=> =not=> fact]]
            [nand-board.logic.board :refer [add-gates
                                            add-pins
                                            add-wires
                                            last-added-gates
                                            last-added-pins
                                            make-initial-board
                                            pins
                                            pins-for-gates]]
            [nand-board.logic.simulator :refer :all]))

(defn- ticks [state]
  (if (pending-events? state)
    (lazy-seq (cons state (ticks (tick state))))
    [state]))

(defn- get-vals [state]
  (->> (pins (:board state))
       (map #(vec [% (get-val state %)]))
       (remove #(nil? (% 1)))
       (into {})))

(defn- get-vals-over-time [state]
  (->> state ticks (map get-vals)))

(fact
  "initial state should have input pins and gateless pins set to 0"
  (let [board (-> (make-initial-board) (add-pins 2) (add-gates 2))
        [p1 p2] (last-added-pins board)
        [i1 i2 _ i4 i5 _] (pins-for-gates board (last-added-gates board))
        state (make-initial-state board)]
    (get-vals state) => {p1 0 p2 0 i1 0 i2 0 i4 0 i5 0}))

(fact
  "initial state should have only unwired input pins set to 0"
  (let [board1 (-> (make-initial-board) (add-gates 2))
        [i1 i2 o3 i4 i5 _] (pins-for-gates board1 (last-added-gates board1))
        board2 (-> board1 (add-wires [o3 i4]))
        state (make-initial-state board2)]
    (get-vals state) => {i1 0 i2 0 i5 0}))

(fact
  "initial state should have wired gateless pins set to 0"
  (let [board1 (-> (make-initial-board) (add-pins 1) (add-gates 1))
        [p1] (last-added-pins board1)
        [i1 i2 _] (pins-for-gates board1 (last-added-gates board1))
        board2 (-> board1 (add-wires [p1 i1]))
        state (make-initial-state board2)]
    (get-vals state) => {p1 0 i2 0}))

(fact
  "propagation delay from input pin to output pin should be 2"
  (let [board (-> (make-initial-board) (add-gates 2))
        [i1 i2 o3 i4 i5 o6] (pins-for-gates board (last-added-gates board))
        vals-over-time (-> (make-initial-state board) get-vals-over-time)]
    vals-over-time => [{i1 0 i2 0 i4 0 i5 0}
                       {i1 0 i2 0 i4 0 i5 0}
                       {i1 0 i2 0 o3 1 i4 0 i5 0 o6 1}]))

(fact
  "propagation delay through a wire should be 1"
  (let [board1 (-> (make-initial-board) (add-pins 1) (add-gates 2))
        [p1] (last-added-pins board1)
        [i1 i2 o3 i4 i5 o6] (pins-for-gates board1 (last-added-gates board1))
        board2 (-> board1 (add-wires [p1 i1] [o3 i4]))
        vals-over-time (-> (make-initial-state board2) get-vals-over-time)]
    vals-over-time => [{p1 0 i2 0 i5 0}
                       {p1 0 i1 0 i2 0 i5 0}
                       {p1 0 i1 0 i2 0 i5 0}
                       {p1 0 i1 0 i2 0 o3 1 i5 0}
                       {p1 0 i1 0 i2 0 o3 1 i4 1 i5 0}
                       {p1 0 i1 0 i2 0 o3 1 i4 1 i5 0}
                       {p1 0 i1 0 i2 0 o3 1 i4 1 i5 0 o6 1}]))

(fact
  "gate should be a NAND"
  (let [board (-> (make-initial-board) (add-gates 1))
        [i1 i2 o3] (pins-for-gates board (last-added-gates board))
        state00 (-> (make-initial-state board) ticks last)
        state10 (-> state00 (set-val i1 1) ticks last)
        state11 (-> state10 (set-val i2 1) ticks last)
        state01 (-> state11 (set-val i1 0) ticks last)]
    (get-vals state00) => {i1 0 i2 0 o3 1}
    (get-vals state10) => {i1 1 i2 0 o3 1}
    (get-vals state11) => {i1 1 i2 1 o3 0}
    (get-vals state01) => {i1 0 i2 1 o3 1}))

(fact
  "changes to both input pins of gate at the same time should propagate as if they were set at the same time"
  (let [board (-> (make-initial-board) (add-gates 1))
        [i1 i2 o3] (pins-for-gates board (last-added-gates board))
        state1 (-> (make-initial-state board) ticks last)
        state2 (-> state1 (set-val i1 1) (set-val i2 1) ticks last)]
    (get-vals state1) => {i1 0 i2 0 o3 1}
    (get-vals state2) => {i1 1 i2 1 o3 0}))

(fact
  "changes to input pins while a propagation is already in process should propagate correctly"
  (let [board (-> (make-initial-board) (add-gates 1))
        [i1 i2 o3] (pins-for-gates board (last-added-gates board))
        vals-over-time (-> (make-initial-state board) tick (set-val i1 1) (set-val i2 1) get-vals-over-time)]
    vals-over-time => [{i1 1 i2 1}
                       {i1 1 i2 1 o3 1}
                       {i1 1 i2 1 o3 0}]))

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
  "events on gateless pin should only propagate if it is a change"
  (let [board1 (-> (make-initial-board) (add-pins 1) (add-gates 1))
        [p1] (last-added-pins board1)
        [i1 _ _] (pins-for-gates board1 (last-added-gates board1))
        board2 (-> board1 (add-wires [p1 i1]))
        state (-> (make-initial-state board2) ticks last)]
    (-> state (set-val p1 0)) =not=> pending-events?
    (-> state (set-val p1 1)) => pending-events?))

(fact
  "output pin can be wired to more than one input pin"
  (let [board1 (-> (make-initial-board) (add-gates 2))
        [i1 i2 o3 i4 i5 o6] (pins-for-gates board1 (last-added-gates board1))
        board2 (-> board1 (add-wires [o3 i4] [o3 i5]))
        state1 (-> (make-initial-state board2) ticks last)
        state2 (-> state1 (set-val i1 1) (set-val i2 1) ticks last)]
    (get-vals state1) => {i1 0 i2 0 o3 1 i4 1, i5 1, o6 0}
    (get-vals state2) => {i1 1 i2 1 o3 0 i4 0, i5 0, o6 1}))

(fact
  "gateless pin can be wired to more than one input pin"
  (let [board1 (-> (make-initial-board) (add-pins 1) (add-gates 1))
        [p1] (last-added-pins board1)
        [i1 i2 o3] (pins-for-gates board1 (last-added-gates board1))
        board2 (-> board1 (add-wires [p1 i1] [p1 i2]))
        state1 (-> (make-initial-state board2) ticks last)
        state2 (-> state1 (set-val p1 1) ticks last)]
    (get-vals state1) => {p1 0 i1 0 i2 0 o3 1}
    (get-vals state2) => {p1 1 i1 1 i2 1 o3 0}))
