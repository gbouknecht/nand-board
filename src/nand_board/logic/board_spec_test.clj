(ns nand-board.logic.board-spec-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [nand-board.logic.board-spec :as board-spec]))

(deftest about-board-data-structure
  (is (not (s/valid? ::board-spec/id -1)))
  (is (s/valid? ::board-spec/id 0))
  (is (s/valid? ::board-spec/id 1))
  (is (s/valid? ::board-spec/id 2))
  (is (not (s/valid? ::board-spec/id "2")))

  (is (not (s/valid? ::board-spec/input-pin-ids #{})))
  (is (not (s/valid? ::board-spec/input-pin-ids #{11})))
  (is (s/valid? ::board-spec/input-pin-ids #{11 13}))
  (is (not (s/valid? ::board-spec/input-pin-ids #{11 13 15})))

  (let [gate {:id 0 :input-pin-ids #{11 13} :output-pin-id 15}]
    (is (s/valid? ::board-spec/gate gate))
    (is (not (s/valid? ::board-spec/gate (assoc-in gate [:output-pin-id] 11))))
    (is (not (s/valid? ::board-spec/gate (assoc-in gate [:output-pin-id] 13))))
    (let [gates {0 gate 1 (assoc gate :id 1)}]
      (is (s/valid? ::board-spec/gates gates))
      (is (not (s/valid? ::board-spec/gates (assoc-in gates [0 :id] 1))))
      (is (not (s/valid? ::board-spec/gates (assoc-in gates [0 :id] 2))))))

  (is (not (s/valid? ::board-spec/wire-ids #{})))
  (is (s/valid? ::board-spec/wire-ids #{0}))
  (is (s/valid? ::board-spec/wire-ids #{0 1}))
  (is (s/valid? ::board-spec/wire-ids #{0 1 2}))

  (let [p1 {:id 0 :gate-id 0}
        p2 {:id 1 :gate-id 2}
        p3 {:id 2}
        pins {0 p1 1 p2 2 p3}]
    (is (s/valid? ::board-spec/pins pins))
    (is (not (s/valid? ::board-spec/pins (assoc-in pins [0 :id] 1))))
    (is (not (s/valid? ::board-spec/pins (assoc-in pins [0 :id] 3))))
    (is (not (s/valid? ::board-spec/pins (assoc-in pins [2 :id] 1))))
    (is (not (s/valid? ::board-spec/pins (assoc-in pins [2 :id] 4)))))

  (let [wire {:id 0 :output-pin-id 0 :input-pin-id 1}]
    (is (s/valid? ::board-spec/wire wire))
    (is (not (s/valid? ::board-spec/wire (assoc wire :input-pin-id 0))))
    (let [wires {0 wire 1 (assoc wire :id 1)}]
      (is (s/valid? ::board-spec/wires wires))
      (is (not (s/valid? ::board-spec/wires (assoc-in wires [0 :id] 1))))
      (is (not (s/valid? ::board-spec/wires (assoc-in wires [0 :id] 2))))))

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
    (is (s/valid? ::board-spec/board {:gates {} :pins {} :wires {} :pin-to-wires {}}))
    (is (s/valid? ::board-spec/board board))

    (testing
      "should validate pin refers to correct gate"
      (is (not (s/valid? ::board-spec/board (assoc-in board [:pins 1 :gate-id] 1))))
      (is (not (s/valid? ::board-spec/board (assoc-in board [:pins 2 :gate-id] 1)))))

    (testing
      "should validate pin wired to correct wire"
      (is (not (s/valid? ::board-spec/board (assoc-in board [:pin-to-wires 1] #{Integer/MAX_VALUE}))))
      (is (not (s/valid? ::board-spec/board (assoc-in board [:wires 1 :output-pin-id] 5))))
      (is (not (s/valid? ::board-spec/board (assoc-in board [:wires 1 :input-pin-id] 0)))))

    (letfn [(add-wire [wire]
              (-> board
                  (assoc-in [:wires (:id wire)] wire)
                  (update-in [:pin-to-wires (:output-pin-id wire)] (fnil conj #{}) (:id wire))
                  (update-in [:pin-to-wires (:input-pin-id wire)] (fnil conj #{}) (:id wire))))]
      (testing
        "should validate input pin not wired by more than one wire"
        (is (not (s/valid? ::board-spec/board (add-wire {:id 9 :output-pin-id 5 :input-pin-id 4})))))

      (testing
        "should validate wire output pin is an 'output' pin"
        (is (not (s/valid? ::board-spec/board (add-wire {:id 9 :output-pin-id 0 :input-pin-id 1})))))

      (testing
        "should validate wire input pin is an 'input' pin"
        (is (not (s/valid? ::board-spec/board (add-wire {:id 9 :output-pin-id 2 :input-pin-id 5}))))
        (is (not (s/valid? ::board-spec/board (add-wire {:id 9 :output-pin-id 2 :input-pin-id 9}))))))))
