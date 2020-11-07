(ns nand-board.logic.board-test
  (:require [clojure.test :refer :all]
            [nand-board.logic.board :refer :all]))

(declare thrown?)

(defn set= [coll1 coll2]
  (= (set coll1) (set coll2)))

(deftest about-retrieval

  (let [board1 (-> (make-initial-board) (add-pins 2) (add-gates 2))
        [p1 p2] (last-added-pins board1)
        [g1 g2] (last-added-gates board1)
        [i1 i2 o3 i4 i5 o6] (pins-for-gates board1 [g1 g2])
        board2 (-> board1 (add-gates 1))
        [g3] (last-added-gates board2)
        [i7 i8 o9] (pins-for-gates board2 [g3])
        board3 (-> board2 (add-wires [o3 i4] [o3 i5] [o6 i1] [p1 i7]))
        [w1 w2 w3 w4] (last-added-wires board3)]
    (is (set= (pins board3) [p1 p2 i1 i2 o3 i4 i5 o6 i7 i8 o9]))
    (is (set= (gateless-pins board3) [p1 p2]))

    (is (set= (gates board3) [g1 g2 g3]))

    (is (= (gate-for-pin board3 i1) g1))
    (is (= (gate-for-pin board3 i2) g1))
    (is (= (gate-for-pin board3 o3) g1))
    (is (= (gate-for-pin board3 i4) g2))
    (is (nil? (gate-for-pin board3 p1)))

    (is (= (input-pins-for-gate board1 g1) [i1 i2]))
    (is (= (input-pins-for-gate board1 g2) [i4 i5]))
    (is (= (output-pin-for-gate board1 g1) o3))
    (is (= (output-pin-for-gate board1 g2) o6))

    (is (not (input-pin? board1 p1)))
    (is (input-pin? board1 i1))
    (is (input-pin? board1 i2))
    (is (not (input-pin? board1 o3)))
    (is (output-pin? board1 p1))
    (is (not (output-pin? board1 i1)))
    (is (not (output-pin? board1 i2)))
    (is (output-pin? board1 o3))

    (is (= (wires-for-pin board3 p1) [w4]))
    (is (= (wires-for-pin board3 i1) [w3]))
    (is (empty? (wires-for-pin board3 i2)))
    (is (set= (wires-for-pin board3 o3) [w1 w2]))
    (is (= (wires-for-pin board3 i4) [w1]))
    (is (= (wires-for-pin board3 i5) [w2]))
    (is (= (wires-for-pin board3 o6) [w3]))

    (is (not (unwired? board3 p1)))
    (is (unwired? board3 p2))
    (is (not (unwired? board3 i1)))
    (is (unwired? board3 i2))
    (is (not (unwired? board3 o3)))
    (is (unwired? board3 o9))

    (is (= (:id (output-pin-for-wire board3 w1)) (:id o3)))
    (is (= (:id (input-pin-for-wire board3 w1)) (:id i4)))))

(deftest about-adding-gates

  (testing
    "should add new gates and pins, no wires"
    (let [board1 (-> (make-initial-board) (add-gates 1))
          [g1] (last-added-gates board1)
          [i1 i2 o3] (pins-for-gates board1 [g1])
          board2 (-> board1 (add-gates 2))
          [g2 g3] (last-added-gates board2)
          [i4 i5 o6 i7 i8 o9] (pins-for-gates board2 [g2 g3])]
      (is (= (input-pins-for-gate board2 g1) [i1 i2]))
      (is (= (output-pin-for-gate board2 g1) o3))
      (is (= (input-pins-for-gate board2 g2) [i4 i5]))
      (is (= (output-pin-for-gate board2 g2) o6))
      (is (= (input-pins-for-gate board2 g3) [i7 i8]))
      (is (= (output-pin-for-gate board2 g3) o9))
      (is (empty? (mapcat #(wires-for-pin board2 %) [i1 i2 o3 i4 i5 o6 i7 i8 o9]))))))

(deftest about-adding-pins

  (testing
    "should add new gateless pins, no wires"
    (let [board1 (-> (make-initial-board) (add-pins 1))
          [p1] (last-added-pins board1)
          board2 (-> board1 (add-pins 2))
          [p2 p3] (last-added-pins board2)]
      (is (= (map keys [p1 p2 p3]) [[:id] [:id] [:id]]))
      (is (= (count (set [p1 p2 p3])) 3))
      (is (empty? (mapcat #(wires-for-pin board2 %) [p1 p2 p3]))))))

(deftest about-adding-wires

  (testing
    "should add wires between pins"
    (let [board1 (-> (make-initial-board) (add-pins 2) (add-gates 3))
          [p1 p2] (last-added-pins board1)
          [i1 i2 o3 i4 i5 _ i7 i8 _] (pins-for-gates board1 (last-added-gates board1))
          board2 (-> board1 (add-wires [p1 i2] [o3 i5] [o3 i1]))
          [w1 w2 w3] (last-added-wires board2)
          board3 (-> board2 (add-wires [o3 i7] [p2 i4] [p2 i8]))
          [w4 w5 w6] (last-added-wires board3)]
      (is (= (wires-for-pin board3 p1) [w1]))
      (is (set= (wires-for-pin board3 p2) [w5 w6]))
      (is (= (wires-for-pin board3 i1) [w3]))
      (is (set= (wires-for-pin board3 o3) [w2 w3 w4]))
      (is (= (wires-for-pin board3 i5) [w2]))
      (is (= (wires-for-pin board3 i7) [w4]))
      (is (= (:id (output-pin-for-wire board3 w2)) (:id o3)))
      (is (= (:id (input-pin-for-wire board3 w2)) (:id i5)))))

  (testing
    "should only accept an 'output' pin and an 'input' pin respectively"
    (let [board (-> (make-initial-board) (add-pins 1) (add-gates 2))
          [p1] (last-added-pins board)
          [i1 i2 o3 _ _ o6] (pins-for-gates board (last-added-gates board))]
      (is (thrown? AssertionError (add-wires board [i2 i1])))
      (is (thrown? AssertionError (add-wires board [o3 o6])))
      (is (thrown? AssertionError (add-wires board [o3 p1]))))))

(deftest about-removing-wires

  (testing
    "should remove wires"
    (let [board1 (-> (make-initial-board) (add-pins 1) (add-gates 3))
          [p1] (last-added-pins board1)
          [i1 i2 o3 i4 i5 _ i7 i8 o9] (pins-for-gates board1 (last-added-gates board1))
          board2 (-> board1 (add-wires [o3 i5] [o3 i4] [o3 i7] [o9 i1] [o3 i2] [p1 i8]))
          [w1 w2 w3 w4 w5 w6] (last-added-wires board2)
          board3 (-> board2 (remove-wires [w1 w3 w6]))]
      (is (empty? (wires-for-pin board3 p1)))
      (is (= (wires-for-pin board3 i1) [w4]))
      (is (= (wires-for-pin board3 i2) [w5]))
      (is (set= (wires-for-pin board3 o3) [w2 w5]))
      (is (= (wires-for-pin board3 i4) [w2]))
      (is (empty? (wires-for-pin board3 i5)))
      (is (empty? (wires-for-pin board3 i7)))
      (is (empty? (wires-for-pin board3 i8)))))

  (testing
    "should do nothing if given wire-ids collection is empty"
    (let [board1 (-> (make-initial-board) (add-gates 1))
          [i1 _ o3] (pins-for-gates board1 (last-added-gates board1))
          board2 (-> board1 (add-wires [o3 i1]))
          board3 (-> board2 (remove-wires []))]
      (is (= board3 board2)))))

(deftest about-removing-gate

  (testing
    "should remove gate, pins and wires"
    (let [board1 (-> (make-initial-board) (add-gates 3))
          [g1 g2 g3] (last-added-gates board1)
          [i1 i2 o3 _ i5 o6 i7 _ _] (pins-for-gates board1 [g1 g2 g3])
          board2 (-> board1 (add-wires [o3 i5] [o3 i7] [o6 i1] [o3 i2]))
          [_ w2 _ w4] (last-added-wires board2)
          board3 (-> board2 (remove-gate g2))]
      (is (empty? (wires-for-pin board3 i1)))
      (is (= (wires-for-pin board3 i2) [w4]))
      (is (set= (wires-for-pin board3 o3) [w2 w4]))
      (is (thrown? AssertionError (wires-for-pin board3 i5)))
      (is (thrown? AssertionError (wires-for-pin board3 o6)))
      (is (= (wires-for-pin board3 i7) [w2]))))

  (testing
    "should be able to remove gate for which output is wired to own input"
    (let [board1 (-> (make-initial-board) (add-gates 1))
          [g1] (last-added-gates board1)
          [i1 _ o3] (pins-for-gates board1 [g1])
          board2 (-> board1 (add-wires [o3 i1]) (remove-gate g1))]
      (is (thrown? AssertionError (wires-for-pin board2 i1)))
      (is (thrown? AssertionError (wires-for-pin board2 o3))))))

(deftest about-removing-pin

  (testing
    "should remove pin and wires"
    (let [board1 (-> (make-initial-board) (add-pins 2) (add-gates 2))
          [p1 p2] (last-added-pins board1)
          [i1 i2 o3 i4 i5 _] (pins-for-gates board1 (last-added-gates board1))
          board2 (-> board1 (add-wires [p1 i1] [p1 i2] [o3 i4] [p2 i5]))
          [_ _ w3 w4] (last-added-wires board2)
          board3 (-> board2 (remove-pin p1))]
      (is (thrown? AssertionError (wires-for-pin board3 p1)))
      (is (empty? (wires-for-pin board3 i1)))
      (is (empty? (wires-for-pin board3 i2)))
      (is (= (wires-for-pin board3 o3) [w3]))
      (is (= (wires-for-pin board3 i4) [w3]))
      (is (= (wires-for-pin board3 p2) [w4]))
      (is (= (wires-for-pin board3 i5) [w4]))))

  (testing
    "should only accept gateless pin"
    (let [board (-> (make-initial-board) (add-gates 1))
          [i1 _ o3] (pins-for-gates board (last-added-gates board))]
      (is (thrown? AssertionError (remove-pin board i1)))
      (is (thrown? AssertionError (remove-pin board o3))))))
