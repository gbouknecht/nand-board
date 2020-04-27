(ns nand-board.logic.board-spec-test
  (:require [clojure.spec.alpha :as s]
            [midje.sweet :refer [=> fact facts]]
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

  (let [gate {:id 0 :input-pin-ids #{11 13} :output-pin-id 15}]
    (s/valid? ::board-spec/gate gate) => true
    (s/valid? ::board-spec/gate (assoc-in gate [:output-pin-id] 11)) => false
    (s/valid? ::board-spec/gate (assoc-in gate [:output-pin-id] 13)) => false
    (let [gates {0 gate 1 (assoc gate :id 1)}]
      (s/valid? ::board-spec/gates gates) => true
      (s/valid? ::board-spec/gates (assoc-in gates [0 :id] 1)) => false
      (s/valid? ::board-spec/gates (assoc-in gates [0 :id] 2)) => false))

  (s/valid? ::board-spec/wire-ids #{}) => false
  (s/valid? ::board-spec/wire-ids #{0}) => true
  (s/valid? ::board-spec/wire-ids #{0 1}) => true
  (s/valid? ::board-spec/wire-ids #{0 1 2}) => true

  (let [p1 {:id 0 :gate-id 0}
        p2 {:id 1 :gate-id 2}
        p3 {:id 2}
        pins {0 p1 1 p2 2 p3}]
    (s/valid? ::board-spec/pins pins) => true
    (s/valid? ::board-spec/pins (assoc-in pins [0 :id] 1)) => false
    (s/valid? ::board-spec/pins (assoc-in pins [0 :id] 3)) => false
    (s/valid? ::board-spec/pins (assoc-in pins [2 :id] 1)) => false
    (s/valid? ::board-spec/pins (assoc-in pins [2 :id] 4)) => false)

  (let [wire {:id 0 :output-pin-id 0 :input-pin-id 1}]
    (s/valid? ::board-spec/wire wire) => true
    (s/valid? ::board-spec/wire (assoc wire :input-pin-id 0)) => false
    (let [wires {0 wire 1 (assoc wire :id 1)}]
      (s/valid? ::board-spec/wires wires) => true
      (s/valid? ::board-spec/wires (assoc-in wires [0 :id] 1)) => false
      (s/valid? ::board-spec/wires (assoc-in wires [0 :id] 2)) => false))

  (let [board {:gates        {0 {:id 0 :input-pin-ids #{0 1} :output-pin-id 2}
                              1 {:id 1 :input-pin-ids #{3 4} :output-pin-id 5}
                              2 {:id 2 :input-pin-ids #{6 7} :output-pin-id 8}}
               :pins         {0 {:id 0 :gate-id 0}
                              1 {:id 1 :gate-id 0}
                              2 {:id 2 :gate-id 0}
                              3 {:id 3 :gate-id 1}
                              4 {:id 4 :gate-id 1}
                              5 {:id 5 :gate-id 1}
                              6 {:id 6 :gate-id 2}
                              7 {:id 7 :gate-id 2}
                              8 {:id 8 :gate-id 2}
                              9 {:id 9}}
               :wires        {0 {:id 0 :output-pin-id 2 :input-pin-id 4}
                              1 {:id 1 :output-pin-id 2 :input-pin-id 6}
                              2 {:id 2 :output-pin-id 5 :input-pin-id 7}
                              3 {:id 3 :output-pin-id 9 :input-pin-id 3}}
               :pin-to-wires {2 #{0 1}
                              3 #{3}
                              4 #{0}
                              5 #{2}
                              6 #{1}
                              7 #{2}
                              9 #{3}}}]
    (s/valid? ::board-spec/board {:gates {} :pins {} :wires {} :pin-to-wires {}}) => true
    (s/valid? ::board-spec/board board) => true

    (fact
      "should validate pin refers to correct gate"
      (s/valid? ::board-spec/board (assoc-in board [:pins 1 :gate-id] 1)) => false
      (s/valid? ::board-spec/board (assoc-in board [:pins 2 :gate-id] 1)) => false)

    (fact
      "should validate pin wired to correct wire"
      (s/valid? ::board-spec/board (assoc-in board [:pin-to-wires 1] #{Integer/MAX_VALUE})) => false
      (s/valid? ::board-spec/board (assoc-in board [:wires 1 :output-pin-id] 5)) => false
      (s/valid? ::board-spec/board (assoc-in board [:wires 1 :input-pin-id] 0)) => false)

    (letfn [(add-wire [wire]
              (-> board
                  (assoc-in [:wires (:id wire)] wire)
                  (update-in [:pin-to-wires (:output-pin-id wire)] (fnil conj #{}) (:id wire))
                  (update-in [:pin-to-wires (:input-pin-id wire)] (fnil conj #{}) (:id wire))))]
      (fact
        "should validate input pin not wired by more than one wire"
        (s/valid? ::board-spec/board (add-wire {:id 9 :output-pin-id 5 :input-pin-id 4})) => false)

      (fact
        "should validate wire output pin is an 'output' pin"
        (s/valid? ::board-spec/board (add-wire {:id 9 :output-pin-id 0 :input-pin-id 1})) => false)

      (fact
        "should validate wire input pin is an 'input' pin"
        (s/valid? ::board-spec/board (add-wire {:id 9 :output-pin-id 2 :input-pin-id 5})) => false
        (s/valid? ::board-spec/board (add-wire {:id 9 :output-pin-id 2 :input-pin-id 9})) => false))))
