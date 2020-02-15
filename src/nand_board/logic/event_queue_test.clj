(ns nand-board.logic.event-queue-test
  (:require [midje.sweet :refer [=> fact]]
            [nand-board.logic.event-queue :refer [add-event
                                                  add-events
                                                  make-event-queue]]))

(fact
  "event-queue should differentiate events on :time and :pin-id only"
  (let [event-queue (-> (make-event-queue) (add-event {:time 0 :pin-id 0 :val 0}))]
    (count (-> event-queue (add-event {:time 1 :pin-id 0 :val 0}))) => 2
    (count (-> event-queue (add-event {:time 0 :pin-id 1 :val 0}))) => 2
    (count (-> event-queue (add-event {:time 0 :pin-id 0 :val 1}))) => 1))

(fact
  "event-queue should sort events on :time and :pin-id ascending"
  (let [event1 {:time 2 :pin-id 1 :val 0}
        event2 {:time 0 :pin-id 1 :val 0}
        event3 {:time 1 :pin-id 1 :val 0}
        event4 {:time 0 :pin-id 2 :val 0}
        event5 {:time 0 :pin-id 0 :val 0}
        event-queue (-> (make-event-queue) (add-events [event1 event2 event3 event4 event5]))]
    (vec event-queue) => [event5 event2 event4 event3 event1]))

(fact
  "adding events to event-queue should replace events with same :time and :pin-id"
  (let [event1 {:time 0 :pin-id 0 :val 0}
        event2 {:time 0 :pin-id 0 :val 1}
        event3 {:time 1 :pin-id 0 :val 0}
        event4 {:time 0 :pin-id 1 :val 0}]
    (vec (-> (make-event-queue) (add-events [event1 event3 event4 event2]))) => [event2 event4 event3]
    (vec (-> (make-event-queue) (add-events [event2 event3 event4 event1]))) => [event1 event4 event3]))
