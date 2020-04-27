(ns nand-board.logic.board-test
  (:require [midje.sweet :refer [=> =not=> contains just fact facts throws]]
            [nand-board.logic.board :refer :all]))

(facts
  "retrieval"
  (let [board1 (-> (make-initial-board) (add-pins 2) (add-gates 2))
        [p1 p2] (last-added-pins board1)
        [g1 g2] (last-added-gates board1)
        [i1 i2 o3 i4 i5 o6] (pins-for-gates board1 [g1 g2])
        board2 (-> board1 (add-gates 1))
        [g3] (last-added-gates board2)
        [i7 i8 o9] (pins-for-gates board2 [g3])
        board3 (-> board2 (add-wires [o3 i4] [o3 i5] [o6 i1] [p1 i7]))
        [w1 w2 w3 w4] (last-added-wires board3)]
    (pins board3) => (just [p1 p2 i1 i2 o3 i4 i5 o6 i7 i8 o9] :in-any-order)
    (gateless-pins board3) => (just [p1 p2] :in-any-order)

    (gates board3) => (just [g1 g2 g3] :in-any-order)

    (gate-for-pin board3 i1) => g1
    (gate-for-pin board3 i2) => g1
    (gate-for-pin board3 o3) => g1
    (gate-for-pin board3 i4) => g2
    (gate-for-pin board3 p1) => nil?

    (input-pins-for-gate board1 g1) => [i1 i2]
    (input-pins-for-gate board1 g2) => [i4 i5]
    (output-pin-for-gate board1 g1) => o3
    (output-pin-for-gate board1 g2) => o6

    (input-pin? board1 p1) => false
    (input-pin? board1 i1) => true
    (input-pin? board1 i2) => true
    (input-pin? board1 o3) => false
    (output-pin? board1 p1) => true
    (output-pin? board1 i1) => false
    (output-pin? board1 i2) => false
    (output-pin? board1 o3) => true

    (wires-for-pin board3 p1) => [w4]
    (wires-for-pin board3 i1) => [w3]
    (wires-for-pin board3 i2) => empty?
    (wires-for-pin board3 o3) => (just [w1 w2] :in-any-order)
    (wires-for-pin board3 i4) => [w1]
    (wires-for-pin board3 i5) => [w2]
    (wires-for-pin board3 o6) => [w3]

    (unwired? board3 p1) => false
    (unwired? board3 p2) => true
    (unwired? board3 i1) => false
    (unwired? board3 i2) => true
    (unwired? board3 o3) => false
    (unwired? board3 o9) => true

    (:id (output-pin-for-wire board3 w1)) => (:id o3)
    (:id (input-pin-for-wire board3 w1)) => (:id i4)))

(facts
  "add-gates"

  (fact
    "should add new gates and pins, no wires"
    (let [board1 (-> (make-initial-board) (add-gates 1))
          [g1] (last-added-gates board1)
          [i1 i2 o3] (pins-for-gates board1 [g1])
          board2 (-> board1 (add-gates 2))
          [g2 g3] (last-added-gates board2)
          [i4 i5 o6 i7 i8 o9] (pins-for-gates board2 [g2 g3])]
      (input-pins-for-gate board2 g1) => [i1 i2]
      (output-pin-for-gate board2 g1) => o3
      (input-pins-for-gate board2 g2) => [i4 i5]
      (output-pin-for-gate board2 g2) => o6
      (input-pins-for-gate board2 g3) => [i7 i8]
      (output-pin-for-gate board2 g3) => o9
      (mapcat #(wires-for-pin board2 %) [i1 i2 o3 i4 i5 o6 i7 i8 o9]) => empty?)))

(facts
  "add-pins"

  (fact
    "should add new gateless pins, no wires"
    (let [board1 (-> (make-initial-board) (add-pins 1))
          [p1] (last-added-pins board1)
          board2 (-> board1 (add-pins 2))
          [p2 p3] (last-added-pins board2)]
      (map keys [p1 p2 p3]) => [[:id] [:id] [:id]]
      (count (set [p1 p2 p3])) => 3
      (mapcat #(wires-for-pin board2 %) [p1 p2 p3]) => empty?)))

(facts
  "add-wires"

  (fact
    "should add wires between pins"
    (let [board1 (-> (make-initial-board) (add-pins 2) (add-gates 3))
          [p1 p2] (last-added-pins board1)
          [i1 i2 o3 i4 i5 _ i7 i8 _] (pins-for-gates board1 (last-added-gates board1))
          board2 (-> board1 (add-wires [p1 i2] [o3 i5] [o3 i1]))
          [w1 w2 w3] (last-added-wires board2)
          board3 (-> board2 (add-wires [o3 i7] [p2 i4] [p2 i8]))
          [w4 w5 w6] (last-added-wires board3)]
      (wires-for-pin board3 p1) => [w1]
      (wires-for-pin board3 p2) => (just [w5 w6] :in-any-order)
      (wires-for-pin board3 i1) => [w3]
      (wires-for-pin board3 o3) => (just [w2 w3 w4] :in-any-order)
      (wires-for-pin board3 i5) => [w2]
      (wires-for-pin board3 i7) => [w4]
      (:id (output-pin-for-wire board3 w2)) => (:id o3)
      (:id (input-pin-for-wire board3 w2)) => (:id i5)))

  (fact
    "should only accept an 'output' pin and an 'input' pin respectively"
    (let [board (-> (make-initial-board) (add-pins 1) (add-gates 2))
          [p1] (last-added-pins board)
          [i1 i2 o3 _ _ o6] (pins-for-gates board (last-added-gates board))]
      (add-wires board [i2 i1]) => (throws AssertionError)
      (add-wires board [o3 o6]) => (throws AssertionError)
      (add-wires board [o3 p1]) => (throws AssertionError))))

(facts
  "remove-wires"

  (fact
    "should remove wires"
    (let [board1 (-> (make-initial-board) (add-pins 1) (add-gates 3))
          [p1] (last-added-pins board1)
          [i1 i2 o3 i4 i5 _ i7 i8 o9] (pins-for-gates board1 (last-added-gates board1))
          board2 (-> board1 (add-wires [o3 i5] [o3 i4] [o3 i7] [o9 i1] [o3 i2] [p1 i8]))
          [w1 w2 w3 w4 w5 w6] (last-added-wires board2)
          board3 (-> board2 (remove-wires [w1 w3 w6]))]
      (wires-for-pin board3 p1) => empty?
      (wires-for-pin board3 i1) => [w4]
      (wires-for-pin board3 i2) => [w5]
      (wires-for-pin board3 o3) => (just [w2 w5] :in-any-order)
      (wires-for-pin board3 i4) => [w2]
      (wires-for-pin board3 i5) => empty?
      (wires-for-pin board3 i7) => empty?
      (wires-for-pin board3 i8) => empty?))

  (fact
    "should do nothing if given wire-ids collection is empty"
    (let [board1 (-> (make-initial-board) (add-gates 1))
          [i1 _ o3] (pins-for-gates board1 (last-added-gates board1))
          board2 (-> board1 (add-wires [o3 i1]))
          board3 (-> board2 (remove-wires []))]
      board3 => board2)))

(facts
  "remove-gate"

  (fact
    "should remove gate, pins and wires"
    (let [board1 (-> (make-initial-board) (add-gates 3))
          [g1 g2 g3] (last-added-gates board1)
          [i1 i2 o3 _ i5 o6 i7 _ _] (pins-for-gates board1 [g1 g2 g3])
          board2 (-> board1 (add-wires [o3 i5] [o3 i7] [o6 i1] [o3 i2]))
          [_ w2 _ w4] (last-added-wires board2)
          board3 (-> board2 (remove-gate g2))]
      (wires-for-pin board3 i1) => empty?
      (wires-for-pin board3 i2) => [w4]
      (wires-for-pin board3 o3) => (just [w2 w4] :in-any-order)
      (wires-for-pin board3 i5) => (throws AssertionError)
      (wires-for-pin board3 o6) => (throws AssertionError)
      (wires-for-pin board3 i7) => [w2]))

  (fact
    "should be able to remove gate for which output is wired to own input"
    (let [board1 (-> (make-initial-board) (add-gates 1))
          [g1] (last-added-gates board1)
          [i1 _ o3] (pins-for-gates board1 [g1])
          board2 (-> board1 (add-wires [o3 i1]) (remove-gate g1))]
      (wires-for-pin board2 i1) => (throws AssertionError)
      (wires-for-pin board2 o3) => (throws AssertionError))))

(facts
  "remove-pin"

  (fact
    "should remove pin and wires"
    (let [board1 (-> (make-initial-board) (add-pins 2) (add-gates 2))
          [p1 p2] (last-added-pins board1)
          [i1 i2 o3 i4 i5 _] (pins-for-gates board1 (last-added-gates board1))
          board2 (-> board1 (add-wires [p1 i1] [p1 i2] [o3 i4] [p2 i5]))
          [_ _ w3 w4] (last-added-wires board2)
          board3 (-> board2 (remove-pin p1))]
      (wires-for-pin board3 p1) => (throws AssertionError)
      (wires-for-pin board3 i1) => empty?
      (wires-for-pin board3 i2) => empty?
      (wires-for-pin board3 o3) => [w3]
      (wires-for-pin board3 i4) => [w3]
      (wires-for-pin board3 p2) => [w4]
      (wires-for-pin board3 i5) => [w4]))

  (fact
    "should only accept gateless pin"
    (let [board (-> (make-initial-board) (add-gates 1))
          [i1 _ o3] (pins-for-gates board (last-added-gates board))]
      (remove-pin board i1) => (throws AssertionError)
      (remove-pin board o3) => (throws AssertionError))))
