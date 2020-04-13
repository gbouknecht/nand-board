(ns nand-board.logic.board-test
  (:require [midje.sweet :refer [=> =not=> contains just fact facts throws]]
            [nand-board.logic.board :refer :all]))

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
  (let [board1 (-> (make-initial-board) (add-gates 2))
        [g1 g2] (last-added-gates board1)
        [i1 i2 o3 i4 i5 o6] (pins-for-gates board1 [g1 g2])
        board2 (-> board1 (add-wires [o3 i4] [o3 i5] [o6 i1]))
        [w1 w2 w3] (last-added-wires board2)]
    [g1 g2] => (map #(get-in board1 [:gates %]) (range 0 2))
    [i1 i2 o3] => (map #(get-in board1 [:pins %]) (range 0 3))
    [i4 i5 o6] => (map #(get-in board1 [:pins %]) (range 3 6))
    [w1 w2 w3] => (map #(get-in board2 [:wires %]) (range 0 3))

    (gate-for-pin board2 i1) => ((:gates board2) 0)
    (gate-for-pin board2 i2) => ((:gates board2) 0)
    (gate-for-pin board2 o3) => ((:gates board2) 0)
    (gate-for-pin board2 i4) => ((:gates board2) 1)

    (pin-for-id board1 (:id i1)) => i1
    (pin-for-id board1 (:id i2)) => i2
    (pin-for-id board1 (:id o3)) => o3
    (pin-for-id board1 (:id i4)) => i4

    (input-pins-for-gate board1 g1) => [i1 i2]
    (input-pins-for-gate board1 g2) => [i4 i5]

    (output-pin-for-gate board1 g1) => o3
    (output-pin-for-gate board1 g2) => o6

    (input-pin? board1 i1) => true
    (input-pin? board1 i2) => true
    (input-pin? board1 o3) => false
    (output-pin? board1 i1) => false
    (output-pin? board1 i2) => false
    (output-pin? board1 o3) => true

    (wires-for-pin board2 i1) => [((:wires board2) 2)]
    (wires-for-pin board2 i2) => empty?
    (wires-for-pin board2 o3) => (just [((:wires board2) 0) ((:wires board2) 1)] :in-any-order)
    (wires-for-pin board2 i4) => [((:wires board2) 0)]
    (wires-for-pin board2 i5) => [((:wires board2) 1)]
    (wires-for-pin board2 o6) => [((:wires board2) 2)]

    (dissoc (output-pin-for-wire board2 w1) :wire-ids) => o3
    (dissoc (input-pin-for-wire board2 w1) :wire-ids) => i4))

(facts
  "add-gates"

  (fact
    "should add new gates and pins, no wires"
    (let [board1 (-> (make-initial-board) (add-gates 1))
          board2 (-> board1 (add-gates 2))
          g1 ((:gates board1) 0)
          g2 ((:gates board2) 1)
          g3 ((:gates board2) 2)]
      g1 => {:id 0 :input-pin-ids #{0 1} :output-pin-id 2}
      (:pins board1) => {0 {:id 0 :gate-id 0}
                         1 {:id 1 :gate-id 0}
                         2 {:id 2 :gate-id 0}}
      (:wires board1) => empty?
      (last-added-gates board1) => [g1]

      ((:gates board2) 0) => g1
      g2 => {:id 1 :input-pin-ids #{3 4} :output-pin-id 5}
      g3 => {:id 2 :input-pin-ids #{6 7} :output-pin-id 8}
      (:pins board2) => (merge (:pins board1)
                               {3 {:id 3 :gate-id 1}
                                4 {:id 4 :gate-id 1}
                                5 {:id 5 :gate-id 1}
                                6 {:id 6 :gate-id 2}
                                7 {:id 7 :gate-id 2}
                                8 {:id 8 :gate-id 2}})
      (:wires board2) => empty?
      (last-added-gates board2) => [g2 g3])))

(facts
  "add-wire"

  (fact
    "should add a wire between two pins"
    (let [board1 (-> (make-initial-board) (add-gates 3))
          [_ _ o3 _ i5 _ i7 _ _] (pins-for-gates board1 (last-added-gates board1))
          board2 (-> board1 (add-wires [o3 i5]))
          board3 (-> board2 (add-wires [o3 i7]))]
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
    "should only accept an 'output' pin and an 'input' pin respectively"
    (let [board (-> (make-initial-board) (add-gates 2))
          [i1 i2 o3 _ _ o6] (pins-for-gates board (last-added-gates board))]
      (add-wires board [i2 i1]) => (throws AssertionError)
      (add-wires board [o3 o6]) => (throws AssertionError))))

(facts
  "remove-wire"

  (fact
    "should remove wire"
    (let [board1 (-> (make-initial-board) (add-gates 3))
          [_ _ o3 i4 i5 _ i7 _ _] (pins-for-gates board1 (last-added-gates board1))
          board2 (-> board1 (add-wires [o3 i5] [o3 i4] [o3 i7]))
          [w1 _ _] (last-added-wires board2)
          board3 (-> board2 (remove-wires [w1]))]
      (:gates board3) => (:gates board1)
      (dissoc (:pins board3) 2 4) => (dissoc (:pins board2) 2 4)
      (set (keys (:wires board3))) => #{1 2}
      ((:pins board3) 2) => {:id 2 :gate-id 0 :wire-ids #{1 2}}
      (keys ((:pins board3) 4)) =not=> (contains :wire-ids)))

  (fact
    "remove-wire may only be called for an existing wire"
    (-> (make-initial-board) (remove-wires [{:id 0 :output-pin-id 2 :input-pin-id 0}])) => (throws AssertionError)))

(facts
  "remove-wires"

  (fact
    "should remove wires"
    (let [board1 (-> (make-initial-board) (add-gates 3))
          [_ _ o3 i4 i5 _ i7 _ _] (pins-for-gates board1 (last-added-gates board1))
          board2 (-> board1 (add-wires [o3 i5] [o3 i4] [o3 i7]))
          [w1 _ w3] (last-added-wires board2)
          board3 (-> board2 (remove-wires [w1 w3]))]
      (:gates board3) => (:gates board1)
      (dissoc (:pins board3) 2 4 6) => (dissoc (:pins board2) 2 4 6)
      (set (keys (:wires board3))) => #{1}
      ((:pins board3) 2) => {:id 2 :gate-id 0 :wire-ids #{1}}
      (keys ((:pins board3) 4)) =not=> (contains :wire-ids)
      (keys ((:pins board3) 6)) =not=> (contains :wire-ids)))

  (fact
    "should do nothing if given wire-ids collection is empty"
    (let [board1 (-> (make-initial-board) (add-gates 1))
          [i1 _ o3] (pins-for-gates board1 (last-added-gates board1))
          board2 (-> board1 (add-wires [o3 i1]))
          board3 (-> board2 (remove-wires []))]
      board3 => board2))

  (fact
    "may only be called for existing and distinct wires"
    (let [board1 (-> (make-initial-board) (add-gates 1))
          [i1 _ o3] (pins-for-gates board1 (last-added-gates board1))
          board2 (-> board1 (add-wires [o3 i1]))
          [w1] (last-added-wires board2)
          w2 {:id 1 :output-pin-id 2 :input-pin-id 0}]
      (remove-wires board2 [w1 w2]) => (throws AssertionError)
      (remove-wires board2 [w1 w1]) => (throws AssertionError))))

(facts
  "remove-gate"

  (fact
    "should remove gate, pins and wires"
    (let [board1 (-> (make-initial-board) (add-gates 3))
          [g1 g2 g3] (last-added-gates board1)
          [_ _ o3 _ i5 _ i7 _ _] (pins-for-gates board1 [g1 g2 g3])
          board2 (-> board1 (add-wires [o3 i5] [o3 i7]))
          board3 (-> board2 (remove-gate g2))]
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
    (let [board1 (-> (make-initial-board) (add-gates 1))
          [g1] (last-added-gates board1)
          [i1 _ o3] (pins-for-gates board1 [g1])
          board2 (-> board1 (add-wires [o3 i1]) (remove-gate g1))]
      (:gates board2) => empty?))

  (fact
    "should be able to remove unwired gate"
    (let [board1 (-> (make-initial-board) (add-gates 1))
          [g1] (last-added-gates board1)
          board2 (-> board1 (remove-gate g1))]
      (:gates board2) => empty?))

  (fact
    "may only be called for an existing gate"
    (let [board (-> (make-initial-board) (add-gates 1))
          g2 {:id 1}]
      (-> board (remove-gate g2))) => (throws AssertionError)))
