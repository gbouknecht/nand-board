(ns nand-board.logic.board-spec-test
  (:require [clojure.spec.alpha :as s]
            [midje.sweet :refer [=> facts]]
            [nand-board.logic.board-spec :as board-spec]))

(facts
  "board data structure"

  (s/valid? ::board-spec/id -1) => false
  (s/valid? ::board-spec/id 0) => true
  (s/valid? ::board-spec/id 1) => true
  (s/valid? ::board-spec/id 2) => true
  (s/valid? ::board-spec/id "2") => false

  (s/valid? ::board-spec/input-pin-ids #{}) => false
  (s/valid? ::board-spec/input-pin-ids #{11}) => false
  (s/valid? ::board-spec/input-pin-ids #{11 13}) => true
  (s/valid? ::board-spec/input-pin-ids #{11 13 15}) => false

  (let [gate {:gate-id 0 :input-pin-ids #{11 13} :output-pin-id 15}]
    (s/valid? ::board-spec/gate gate) => true
    (s/valid? ::board-spec/gate (assoc-in gate [:output-pin-id] 11)) => false
    (s/valid? ::board-spec/gate (assoc-in gate [:output-pin-id] 13)) => false
    (let [gates {0 gate 1 (assoc gate :gate-id 1)}]
      (s/valid? ::board-spec/gates gates) => true
      (s/valid? ::board-spec/gates (assoc-in gates [0 :gate-id] 1)) => false
      (s/valid? ::board-spec/gates (assoc-in gates [0 :gate-id] 2)) => false))

  (s/valid? ::board-spec/wire-ids #{}) => false
  (s/valid? ::board-spec/wire-ids #{0}) => true
  (s/valid? ::board-spec/wire-ids #{0 1}) => true
  (s/valid? ::board-spec/wire-ids #{0 1 2}) => true
  (s/valid? ::board-spec/wire-ids #{0 -1 2}) => false
  (s/valid? ::board-spec/wire-ids #{0 "1" 2}) => false

  (let [pin {:pin-id 0 :gate-id 0}
        pins {0 pin 1 (assoc pin :pin-id 1)}]
    (s/valid? ::board-spec/pins pins) => true
    (s/valid? ::board-spec/pins (assoc-in pins [0 :pin-id] 1)) => false
    (s/valid? ::board-spec/pins (assoc-in pins [0 :pin-id] 2)) => false)

  (let [wire {:wire-id 0 :output-pin-id 0 :input-pin-id 1}]
    (s/valid? ::board-spec/wire wire) => true
    (s/valid? ::board-spec/wire (assoc wire :input-pin-id 0)) => false
    (let [wires {0 wire 1 (assoc wire :wire-id 1)}]
      (s/valid? ::board-spec/wires wires) => true
      (s/valid? ::board-spec/wires (assoc-in wires [0 :wire-id] 1)) => false
      (s/valid? ::board-spec/wires (assoc-in wires [0 :wire-id] 2)) => false))

  (let [board {:gates {0 {:gate-id 0 :input-pin-ids #{0 1} :output-pin-id 2}
                       1 {:gate-id 1 :input-pin-ids #{3 4} :output-pin-id 5}
                       2 {:gate-id 2 :input-pin-ids #{6 7} :output-pin-id 8}}
               :pins  {0 {:pin-id 0 :gate-id 0}
                       1 {:pin-id 1 :gate-id 0}
                       2 {:pin-id 2 :gate-id 0 :wire-ids #{0 1}}
                       3 {:pin-id 3 :gate-id 1}
                       4 {:pin-id 4 :gate-id 1 :wire-ids #{0}}
                       5 {:pin-id 5 :gate-id 1 :wire-ids #{2}}
                       6 {:pin-id 6 :gate-id 2 :wire-ids #{1}}
                       7 {:pin-id 7 :gate-id 2 :wire-ids #{2}}
                       8 {:pin-id 8 :gate-id 2}}
               :wires {0 {:wire-id 0 :output-pin-id 2 :input-pin-id 4}
                       1 {:wire-id 1 :output-pin-id 2 :input-pin-id 6}
                       2 {:wire-id 2 :output-pin-id 5 :input-pin-id 7}}}]
    (s/valid? ::board-spec/board {:gates {} :pins {} :wires {}}) => true
    (s/valid? ::board-spec/board board) => true
    (s/valid? ::board-spec/board (assoc-in board [:pins 1 :gate-id] 1)) => false
    (s/valid? ::board-spec/board (assoc-in board [:pins 2 :gate-id] 1)) => false
    (s/valid? ::board-spec/board (assoc-in board [:pins 1 :wire-ids] #{Integer/MAX_VALUE})) => false
    (s/valid? ::board-spec/board (assoc-in board [:wires 1 :output-pin-id] 5)) => false
    (s/valid? ::board-spec/board (assoc-in board [:wires 1 :input-pin-id] 0)) => false
    (s/valid? ::board-spec/board (-> board
                                     (assoc-in [:wires 3] {:wire-id 3 :output-pin-id 5 :input-pin-id 4})
                                     (update-in [:pins 5 :wire-ids] conj 3)
                                     (update-in [:pins 4 :wire-ids] conj 3))) => false
    (s/valid? ::board-spec/board (-> board
                                     (assoc-in [:wires 3] {:wire-id 3 :output-pin-id 0 :input-pin-id 1})
                                     (assoc-in [:pins 0 :wire-ids] #{3})
                                     (assoc-in [:pins 1 :wire-ids] #{3}))) => false
    (s/valid? ::board-spec/board (-> board
                                     (assoc-in [:wires 3] {:wire-id 3 :output-pin-id 2 :input-pin-id 5})
                                     (update-in [:pins 2 :wire-ids] conj 3)
                                     (update-in [:pins 5 :wire-ids] conj 3))) => false))
