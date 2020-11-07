(ns nand-board.logic.event-queue-test
  (:require [clojure.test :refer :all]
            [nand-board.logic.event-queue :refer :all]))

(defn- make-pin [id]
  {:id id :gate-id 0})

(deftest about-event-queue

  (testing
    "should differentiate events on :time and :pin-id only"
    (let [event-queue (-> (make-event-queue) (add-event {:time 0 :pin (make-pin 0) :val 0}))]
      (is (= (count (-> event-queue (add-event {:time 1 :pin (make-pin 0) :val 0}))) 2))
      (is (= (count (-> event-queue (add-event {:time 0 :pin (make-pin 1) :val 0}))) 2))
      (is (= (count (-> event-queue (add-event {:time 0 :pin (make-pin 0) :val 1}))) 1))))

  (testing
    "should sort events on :time and :pin-id ascending"
    (let [event1 {:time 2 :pin (make-pin 1) :val 0}
          event2 {:time 0 :pin (make-pin 1) :val 0}
          event3 {:time 1 :pin (make-pin 1) :val 0}
          event4 {:time 0 :pin (make-pin 2) :val 0}
          event5 {:time 0 :pin (make-pin 0) :val 0}
          event-queue (-> (make-event-queue) (add-events [event1 event2 event3 event4 event5]))]
      (is (= (vec event-queue) [event5 event2 event4 event3 event1]))))

  (testing
    "adding events should replace events with same :time and :pin-id"
    (let [event1 {:time 0 :pin (make-pin 0) :val 0}
          event2 {:time 0 :pin (make-pin 0) :val 1}
          event3 {:time 1 :pin (make-pin 0) :val 0}
          event4 {:time 0 :pin (make-pin 1) :val 0}]
      (is (= (vec (-> (make-event-queue) (add-events [event1 event3 event4 event2]))) [event2 event4 event3]))
      (is (= (vec (-> (make-event-queue) (add-events [event2 event3 event4 event1]))) [event1 event4 event3])))))
