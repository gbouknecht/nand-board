(ns nand-board.ui.ui-state-test
  (:require [midje.sweet :refer [=> fact]]
            [nand-board.logic.board :refer [gates]]
            [nand-board.ui.ui-state :refer :all]))

(defn single-clicked-called [ui-state event]
  (update ui-state :single-clicked-calls (fnil conj []) event))

(defn double-clicked-called [ui-state event]
  (update ui-state :double-clicked-calls (fnil conj []) event))

(fact
  "initial ui-state should not have recognized any click events"
  (let [ui-state (-> (make-initial-ui-state 1000
                                            :single-clicked single-clicked-called
                                            :double-clicked double-clicked-called))]
    (:single-clicked-calls ui-state) => nil
    (:double-clicked-calls ui-state) => nil))

(fact
  "should recognize single click event"
  (let [event {:x 2 :y 3}
        ui-state-1000 (-> (make-initial-ui-state 1000 :single-clicked single-clicked-called)
                          (add-click-event event))
        ui-state-1499 (-> ui-state-1000 (update-time-ms 1499))
        ui-state-1500 (-> ui-state-1000 (update-time-ms 1500))
        ui-state-1501 (-> ui-state-1000 (update-time-ms 1501))]
    (:single-clicked-calls ui-state-1000) => nil
    (:single-clicked-calls ui-state-1499) => nil
    (:single-clicked-calls ui-state-1500) => nil
    (:single-clicked-calls ui-state-1501) => [event]))

(fact
  "should recognize two single click events right after each other"
  (let [event1 {:x 2 :y 3}
        event2 {:x 4 :y 5}
        ui-state (-> (make-initial-ui-state 1000 :single-clicked single-clicked-called)
                     (add-click-event event1)
                     (update-time-ms 1501) (add-click-event event2)
                     (update-time-ms 2002))]
    (:single-clicked-calls ui-state) => [event1 event2]))

(fact
  "should recognize single click event only once"
  (let [event {:x 2 :y 3}
        ui-state (-> (make-initial-ui-state 1000 :single-clicked single-clicked-called)
                     (add-click-event event)
                     (update-time-ms 2002))]
    (:single-clicked-calls ui-state) => [event]))

(fact
  "double click event should not be recognized as single click event"
  (let [event {:x 2 :y 3}
        ui-state (-> (make-initial-ui-state 1000 :single-clicked single-clicked-called)
                     (add-click-event event)
                     (update-time-ms 1100) (add-click-event event)
                     (update-time-ms 1501))]
    (:single-clicked-calls ui-state) => nil))

(fact
  "should recognize double click event"
  (let [event {:x 2 :y 3}
        ui-state-1000 (-> (make-initial-ui-state 1000 :double-clicked double-clicked-called)
                          (add-click-event event))
        ui-state-1001 (-> ui-state-1000 (update-time-ms 1001) (add-click-event event))
        ui-state-1499 (-> ui-state-1000 (update-time-ms 1499) (add-click-event event))
        ui-state-1500 (-> ui-state-1000 (update-time-ms 1500) (add-click-event event))
        ui-state-1501 (-> ui-state-1000 (update-time-ms 1501) (add-click-event event))]
    (:double-clicked-calls ui-state-1000) => nil
    (:double-clicked-calls ui-state-1001) => [event]
    (:double-clicked-calls ui-state-1499) => [event]
    (:double-clicked-calls ui-state-1500) => [event]
    (:double-clicked-calls ui-state-1501) => nil))

(fact
  "should recognize two double click events right after each other"
  (let [event1 {:x 2 :y 3}
        event2 {:x 4 :y 5}
        ui-state-1 (-> (make-initial-ui-state 1000 :double-clicked double-clicked-called)
                       (add-click-event event1)
                       (update-time-ms 1100) (add-click-event event1)
                       (update-time-ms 1200) (add-click-event event2)
                       (update-time-ms 1300) (add-click-event event2))]
    (:double-clicked-calls ui-state-1) => [event1 event2]))

(fact
  "should recognize double click event only once"
  (let [event {:x 2 :y 3}
        ui-state (-> (make-initial-ui-state 1000 :double-clicked double-clicked-called)
                     (add-click-event event)
                     (update-time-ms 1100) (add-click-event event)
                     (update-time-ms 1200))]
    (:double-clicked-calls ui-state) => [event]))

(fact
  "add-gate should add a gate on the specified coordinates"
  (let [ui-state (-> (make-initial-ui-state 1000) (add-gate-view [2 3]) (add-gate-view [4 5]))
        gate-views (gate-views ui-state)
        gates (gates (:board (:state ui-state)))]
    (map :gate gate-views) => gates
    (map :center gate-views) => [[2 3] [4 5]]))
