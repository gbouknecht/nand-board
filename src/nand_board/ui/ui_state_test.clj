(ns nand-board.ui.ui-state-test
  (:require [clojure.test :refer :all]
            [nand-board.logic.board :refer [gates
                                            output-pin-for-gate]]
            [nand-board.logic.simulator :refer [get-val]]
            [nand-board.ui.ui-state :refer :all]))

(defn single-clicked [ui-state event]
  (update ui-state :single-clicked-calls (fnil conj []) event))

(defn double-clicked [ui-state event]
  (update ui-state :double-clicked-calls (fnil conj []) event))

(deftest about-initialization

  (testing
    "initial ui-state should have defaults
     - time-ms: 0
     - tick-interval-ms: 250"
    (is (= (:time-ms (make-initial-ui-state)) 0))
    (is (= (:tick-interval-ms (make-initial-ui-state)) 250)))

  (testing
    "initial ui-state should not have recognized any click events"
    (let [ui-state (-> (make-initial-ui-state :time-ms 1000
                                              :single-clicked single-clicked
                                              :double-clicked double-clicked))]
      (is (nil? (:single-clicked-calls ui-state)))
      (is (nil? (:double-clicked-calls ui-state))))))

(deftest about-single-clicking

  (testing
    "should recognize single click event"
    (let [event {:x 200 :y 300}
          ui-state-1000 (-> (make-initial-ui-state :time-ms 1000
                                                   :single-clicked single-clicked)
                            (add-click-event event))
          ui-state-1499 (-> ui-state-1000 (update-time-ms 1499))
          ui-state-1500 (-> ui-state-1000 (update-time-ms 1500))
          ui-state-1501 (-> ui-state-1000 (update-time-ms 1501))]
      (is (nil? (:single-clicked-calls ui-state-1000)))
      (is (nil? (:single-clicked-calls ui-state-1499)))
      (is (nil? (:single-clicked-calls ui-state-1500)))
      (is (= (:single-clicked-calls ui-state-1501) [event]))))

  (testing
    "should recognize two single click events right after each other"
    (let [event1 {:x 200 :y 300}
          event2 {:x 400 :y 500}
          ui-state (-> (make-initial-ui-state :time-ms 1000
                                              :single-clicked single-clicked)
                       (add-click-event event1)
                       (update-time-ms 1501) (add-click-event event2)
                       (update-time-ms 2002))]
      (is (= (:single-clicked-calls ui-state) [event1 event2]))))

  (testing
    "should recognize single click event only once"
    (let [event {:x 200 :y 300}
          ui-state (-> (make-initial-ui-state :time-ms 1000
                                              :single-clicked single-clicked)
                       (add-click-event event)
                       (update-time-ms 2002))]
      (is (= (:single-clicked-calls ui-state) [event]))))

  (testing
    "double click event should not be recognized as single click event"
    (let [event {:x 200 :y 300}
          ui-state (-> (make-initial-ui-state :time-ms 1000
                                              :single-clicked single-clicked)
                       (add-click-event event)
                       (update-time-ms 1100) (add-click-event event)
                       (update-time-ms 1501))]
      (is (nil? (:single-clicked-calls ui-state))))))

(deftest about-double-clicking

  (testing
    "should recognize double click event"
    (let [event {:x 200 :y 300}
          ui-state-1000 (-> (make-initial-ui-state :time-ms 1000
                                                   :double-clicked double-clicked)
                            (add-click-event event))
          ui-state-1001 (-> ui-state-1000 (update-time-ms 1001) (add-click-event event))
          ui-state-1499 (-> ui-state-1000 (update-time-ms 1499) (add-click-event event))
          ui-state-1500 (-> ui-state-1000 (update-time-ms 1500) (add-click-event event))
          ui-state-1501 (-> ui-state-1000 (update-time-ms 1501) (add-click-event event))]
      (is (nil? (:double-clicked-calls ui-state-1000)))
      (is (= (:double-clicked-calls ui-state-1001) [event]))
      (is (= (:double-clicked-calls ui-state-1499) [event]))
      (is (= (:double-clicked-calls ui-state-1500) [event]))
      (is (nil? (:double-clicked-calls ui-state-1501)))))

  (testing
    "should recognize two double click events right after each other"
    (let [event1 {:x 200 :y 300}
          event2 {:x 400 :y 500}
          ui-state-1 (-> (make-initial-ui-state :time-ms 1000
                                                :double-clicked double-clicked)
                         (add-click-event event1)
                         (update-time-ms 1100) (add-click-event event1)
                         (update-time-ms 1200) (add-click-event event2)
                         (update-time-ms 1300) (add-click-event event2))]
      (is (= (:double-clicked-calls ui-state-1) [event1 event2]))))

  (testing
    "should recognize double click event only once"
    (let [event {:x 200 :y 300}
          ui-state (-> (make-initial-ui-state :time-ms 1000
                                              :double-clicked double-clicked)
                       (add-click-event event)
                       (update-time-ms 1100) (add-click-event event)
                       (update-time-ms 1200))]
      (is (= (:double-clicked-calls ui-state) [event])))))

(deftest about-adding-gate-views

  (testing
    "should add a gate at the specified coordinates"
    (let [ui-state (-> (make-initial-ui-state :time-ms 1000)
                       (add-gate-view-if-no-overlaps [200 300])
                       (add-gate-view-if-no-overlaps [400 500]))
          gate-views (gate-views ui-state)
          gates (gates (:board (:state ui-state)))]
      (is (= (map :gate gate-views) gates))
      (is (= (map :center gate-views) [[200 300] [400 500]]))))

  (testing
    "should not add a gate if it overlaps another gate"
    (let [ui-state (-> (make-initial-ui-state :time-ms 1000)
                       (add-gate-view-if-no-overlaps [200 300])
                       (add-gate-view-if-no-overlaps [250 350]))
          gate-views (gate-views ui-state)
          gates (gates (:board (:state ui-state)))]
      (is (= (map :gate gate-views) gates))
      (is (= (map :center gate-views) [[200 300]])))))

(deftest about-finding-view-at-coords

  (testing
    "should find view that contains given coords"
    (let [ui-state (-> (make-initial-ui-state :time-ms 1000)
                       (add-gate-view-if-no-overlaps [200 300])
                       (add-gate-view-if-no-overlaps [400 500]))
          [view1 view2] (gate-views ui-state)]
      (is (= (view-at-coords ui-state [210 285]) view1))
      (is (= (view-at-coords ui-state [390 505]) view2))
      (is (nil? (view-at-coords ui-state [600 700]))))))

(deftest about-ticking-state

  (testing
    "should tick state at specified rate"
    (let [ui-state (-> (make-initial-ui-state :time-ms 1000 :tick-interval-ms 500)
                       (add-gate-view-if-no-overlaps [200 300]))
          [gate] (map :gate (gate-views ui-state))
          get-val-output-pin (fn [{:keys [state]} gate] (get-val state (output-pin-for-gate (:board state) gate)))]
      (is (nil? (get-val-output-pin ui-state gate)))
      (is (nil? (get-val-output-pin (update-time-ms ui-state 1999) gate)))
      (is (= (get-val-output-pin (update-time-ms ui-state 2000) gate) 1)))))
