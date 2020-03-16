(ns nand-board.logic.board-test
  (:require [midje.sweet :refer [=> =not=> contains just fact facts throws]]
            [nand-board.logic.board :refer [add-gate
                                            add-wire
                                            gate-for-pin-id
                                            make-initial-board
                                            remove-gate
                                            remove-wire
                                            remove-wires
                                            wires-for-pin-id]]))

(facts
  "make-initial-board"

  (fact
    "should give a board with no gates, pins and wires"
    (let [board (-> (make-initial-board))]
      (:gates board) => empty?
      (:pins board) => empty?
      (:wires board) => empty?)))

(facts
  "retrieval"
  (let [board (-> (make-initial-board)
                  add-gate add-gate
                  (add-wire 2 3) (add-wire 2 4) (add-wire 5 0))]
    (gate-for-pin-id board 0) => ((:gates board) 0)
    (gate-for-pin-id board 1) => ((:gates board) 0)
    (gate-for-pin-id board 2) => ((:gates board) 0)
    (gate-for-pin-id board 3) => ((:gates board) 1)

    (wires-for-pin-id board 0) => [((:wires board) 2)]
    (wires-for-pin-id board 1) => empty?
    (wires-for-pin-id board 2) => (just [((:wires board) 0) ((:wires board) 1)] :in-any-order)
    (wires-for-pin-id board 3) => [((:wires board) 0)]
    (wires-for-pin-id board 4) => [((:wires board) 1)]
    (wires-for-pin-id board 5) => [((:wires board) 2)]))

(facts
  "add-gate"

  (fact
    "should add a new gate and pins, no wires"
    (let [board1 (-> (make-initial-board) add-gate)
          board2 (-> board1 add-gate)]
      ((:gates board1) 0) => {:id 0 :input-pin-ids #{0 1} :output-pin-id 2}
      (:pins board1) => {0 {:id 0 :gate-id 0}
                         1 {:id 1 :gate-id 0}
                         2 {:id 2 :gate-id 0}}
      (:wires board1) => empty?

      ((:gates board2) 0) => ((:gates board1) 0)
      ((:gates board2) 1) => {:id 1 :input-pin-ids #{3 4} :output-pin-id 5}
      (:pins board2) => (merge (:pins board1)
                               {3 {:id 3 :gate-id 1}
                                4 {:id 4 :gate-id 1}
                                5 {:id 5 :gate-id 1}})
      (:wires board2) => empty?)))

(facts
  "add-wire"

  (fact
    "should add a wire between two pins"
    (let [board1 (-> (make-initial-board) add-gate add-gate add-gate)
          board2 (-> board1 (add-wire 2 4))
          board3 (-> board2 (add-wire 2 6))]
      (:gates board2) => (:gates board1)
      (dissoc (:pins board2) 2 4) => (dissoc (:pins board1) 2 4)
      ((:pins board2) 2) => {:id 2 :gate-id 0 :wire-ids #{0}}
      ((:pins board2) 4) => {:id 4 :gate-id 1 :wire-ids #{0}}
      ((:wires board2) 0) => {:id 0 :output-pin-id 2 :input-pin-id 4}

      (:gates board3) => (:gates board1)
      (dissoc (:pins board3) 2 6) => (dissoc (:pins board2) 2 6)
      ((:pins board3) 2) => {:id 2 :gate-id 0 :wire-ids #{0 1}}
      ((:pins board3) 6) => {:id 6 :gate-id 2 :wire-ids #{1}}
      ((:wires board3) 1) => {:id 1 :output-pin-id 2 :input-pin-id 6}))

  (fact
    "should only accept an 'output' pin-id and an 'input' pin-id respectively"
    (let [board (-> (make-initial-board) add-gate add-gate)]
      (add-wire board 1 0) => (throws AssertionError)
      (add-wire board 8 0) => (throws AssertionError)
      (add-wire board 2 5) => (throws AssertionError)
      (add-wire board 2 6) => (throws AssertionError))))

(facts
  "remove-wire"

  (fact
    "should remove wire"
    (let [board1 (-> (make-initial-board) add-gate add-gate add-gate)
          board2 (-> board1 (add-wire 2 4) (add-wire 2 3) (add-wire 2 6))
          board3 (-> board2 (remove-wire 0))]
      (:gates board3) => (:gates board1)
      (dissoc (:pins board3) 2 4) => (dissoc (:pins board2) 2 4)
      (set (keys (:wires board3))) => #{1 2}
      ((:pins board3) 2) => {:id 2 :gate-id 0 :wire-ids #{1 2}}
      (keys ((:pins board3) 4)) =not=> (contains :wire-ids)))

  (fact
    "remove-wire may only be called for an existing wire"
    (-> (make-initial-board) (remove-wire 0)) => (throws AssertionError)))

(facts
  "remove-wires"

  (fact
    "should remove wires"
    (let [board1 (-> (make-initial-board) add-gate add-gate add-gate)
          board2 (-> board1 (add-wire 2 4) (add-wire 2 3) (add-wire 2 6))
          board3 (-> board2 (remove-wires [0 2]))]
      (:gates board3) => (:gates board1)
      (dissoc (:pins board3) 2 4 6) => (dissoc (:pins board2) 2 4 6)
      (set (keys (:wires board3))) => #{1}
      ((:pins board3) 2) => {:id 2 :gate-id 0 :wire-ids #{1}}
      (keys ((:pins board3) 4)) =not=> (contains :wire-ids)
      (keys ((:pins board3) 6)) =not=> (contains :wire-ids)))

  (fact
    "should do nothing if given wire-ids collection is empty"
    (let [board1 (-> (make-initial-board) add-gate (add-wire 2 0))
          board2 (-> board1 (remove-wires []))]
      board2 => board1))

  (fact
    "may only be called for existing and distinct wires"
    (let [board (-> (make-initial-board) add-gate (add-wire 2 0))]
      (remove-wires board [0 1]) => (throws AssertionError)
      (remove-wires board [0 0]) => (throws AssertionError))))

(facts
  "remove-gate"

  (fact
    "should remove gate, pins and wires"
    (let [board1 (-> (make-initial-board) add-gate add-gate add-gate)
          board2 (-> board1 (add-wire 2 4) (add-wire 2 6))
          board3 (-> board2 (remove-gate 1))]
      (dissoc (:gates board3) 1) => (dissoc (:gates board1) 1)
      (dissoc (:pins board3) 2 3 4 5) => (dissoc (:pins board2) 2 3 4 5)
      (dissoc (:wires board3) 0) => (dissoc (:wires board2) 0)
      (set (keys (:gates board3))) => #{0 2}
      (set (keys (:pins board3))) => #{0 1 2 6 7 8}
      (set (keys (:wires board3))) => #{1}
      ((:pins board3) 2) => {:id 2 :gate-id 0 :wire-ids #{1}}
      ((:pins board3) 6) => {:id 6 :gate-id 2 :wire-ids #{1}}))

  (fact
    "should be able to remove gate for which output is wired to own input"
    (let [board (-> (make-initial-board) add-gate (add-wire 2 0) (remove-gate 0))]
      (:gates board) => empty?))

  (fact
    "should be able to remove unwired gate"
    (let [board (-> (make-initial-board) add-gate (remove-gate 0))]
      (:gates board) => empty?))

  (fact
    "may only be called for an existing gate"
    (-> (make-initial-board) add-gate (remove-gate 1)) => (throws AssertionError)))
