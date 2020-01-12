(ns nand-board.logic.board-test
  (:require [midje.sweet :refer [=> =not=> contains fact throws]]
            [nand-board.logic.board :refer [add-gate
                                            add-wire
                                            make-initial-board
                                            remove-gate
                                            remove-wire
                                            remove-wires]]))

(fact
  "make-initial-board should give a board with no gates, pins and wires"
  (let [board (-> (make-initial-board))]
    (:gates board) => empty?
    (:pins board) => empty?
    (:wires board) => empty?))

(fact
  "add-gate should add a new gate and pins, no wires"
  (let [board1 (-> (make-initial-board) add-gate)
        board2 (-> board1 add-gate)]
    ((:gates board1) 0) => {:gate-id 0 :inputs [{:pin-id 0} {:pin-id 1}] :output {:pin-id 2}}
    (:pins board1) => {0 {:pin-id 0 :gate-id 0}
                       1 {:pin-id 1 :gate-id 0}
                       2 {:pin-id 2 :gate-id 0}}
    (:wires board1) => empty?

    ((:gates board2) 0) => ((:gates board1) 0)
    ((:gates board2) 1) => {:gate-id 1 :inputs [{:pin-id 3} {:pin-id 4}] :output {:pin-id 5}}
    (:pins board2) => (merge (:pins board1)
                             {3 {:pin-id 3 :gate-id 1}
                              4 {:pin-id 4 :gate-id 1}
                              5 {:pin-id 5 :gate-id 1}})
    (:wires board2) => empty?))

(fact
  "add-wire should add a wire between two pins"
  (let [board1 (-> (make-initial-board) add-gate add-gate add-gate)
        board2 (-> board1 (add-wire 2 4))
        board3 (-> board2 (add-wire 2 6))]
    (:gates board2) => (:gates board1)
    (dissoc (:pins board2) 2 4) => (dissoc (:pins board1) 2 4)
    ((:pins board2) 2) => {:pin-id 2 :gate-id 0 :wire-ids #{0}}
    ((:pins board2) 4) => {:pin-id 4 :gate-id 1 :wire-ids #{0}}
    ((:wires board2) 0) => {:wire-id 0 :output-pin-id 2 :input-pin-id 4}

    (:gates board3) => (:gates board1)
    (dissoc (:pins board3) 2 6) => (dissoc (:pins board2) 2 6)
    ((:pins board3) 2) => {:pin-id 2 :gate-id 0 :wire-ids #{0 1}}
    ((:pins board3) 6) => {:pin-id 6 :gate-id 2 :wire-ids #{1}}
    ((:wires board3) 1) => {:wire-id 1 :output-pin-id 2 :input-pin-id 6}))

(fact
  "add-wire should only accept an 'output' pin-id and an 'input' pin-id respectively"
  (let [board (-> (make-initial-board) add-gate add-gate)]
    (add-wire board 1 0) => (throws AssertionError)
    (add-wire board 8 0) => (throws AssertionError)
    (add-wire board 2 5) => (throws AssertionError)
    (add-wire board 2 6) => (throws AssertionError)))

(fact
  "remove-gate should remove gate, pins and wires"
  (let [board1 (-> (make-initial-board) add-gate add-gate add-gate)
        board2 (-> board1 (add-wire 2 4) (add-wire 2 6))
        board3 (-> board2 (remove-gate 1))]
    (dissoc (:gates board3) 1) => (dissoc (:gates board1) 1)
    (dissoc (:pins board3) 2 3 4 5) => (dissoc (:pins board2) 2 3 4 5)
    (dissoc (:wires board3) 0) => (dissoc (:wires board2) 0)
    (set (keys (:gates board3))) => #{0 2}
    (set (keys (:pins board3))) => #{0 1 2 6 7 8}
    (set (keys (:wires board3))) => #{1}
    ((:pins board3) 2) => {:pin-id 2 :gate-id 0 :wire-ids #{1}}
    ((:pins board3) 6) => {:pin-id 6 :gate-id 2 :wire-ids #{1}}))

(fact
  "remove-gate should be able to remove gate for which output is wired to own input"
  (let [board (-> (make-initial-board) add-gate (add-wire 2 0) (remove-gate 0))]
    (:gates board) => empty?))

(fact
  "remove-gate should be able to remove unwired gate"
  (let [board (-> (make-initial-board) add-gate (remove-gate 0))]
    (:gates board) => empty?))

(fact
  "remove-gate may only be called for an existing gate"
  (-> (make-initial-board) add-gate (remove-gate 1)) => (throws AssertionError))

(fact
  "remove-wire should remove wire"
  (let [board1 (-> (make-initial-board) add-gate add-gate add-gate)
        board2 (-> board1 (add-wire 2 4) (add-wire 2 3) (add-wire 2 6))
        board3 (-> board2 (remove-wire 0))]
    (:gates board3) => (:gates board1)
    (dissoc (:pins board3) 2 4) => (dissoc (:pins board2) 2 4)
    (set (keys (:wires board3))) => #{1 2}
    ((:pins board3) 2) => {:pin-id 2 :gate-id 0 :wire-ids #{1 2}}
    (keys ((:pins board3) 4)) =not=> (contains :wire-ids)))

(fact
  "remove-wire may only be called for an existing wire"
  (-> (make-initial-board) (remove-wire 0)) => (throws AssertionError))

(fact
  "remove-wires should remove wires"
  (let [board1 (-> (make-initial-board) add-gate add-gate add-gate)
        board2 (-> board1 (add-wire 2 4) (add-wire 2 3) (add-wire 2 6))
        board3 (-> board2 (remove-wires [0 2]))]
    (:gates board3) => (:gates board1)
    (dissoc (:pins board3) 2 4 6) => (dissoc (:pins board2) 2 4 6)
    (set (keys (:wires board3))) => #{1}
    ((:pins board3) 2) => {:pin-id 2 :gate-id 0 :wire-ids #{1}}
    (keys ((:pins board3) 4)) =not=> (contains :wire-ids)
    (keys ((:pins board3) 6)) =not=> (contains :wire-ids)))

(fact
  "remove-wires should do nothing if given wire-ids collection is empty"
  (let [board1 (-> (make-initial-board) add-gate (add-wire 2 0))
        board2 (-> board1 (remove-wires []))]
    board2 => board1))

(fact
  "remove-wires may only be called for existing and distinct wires"
  (let [board (-> (make-initial-board) add-gate (add-wire 2 0))]
    (remove-wires board [0 1]) => (throws AssertionError)
    (remove-wires board [0 0]) => (throws AssertionError)))
