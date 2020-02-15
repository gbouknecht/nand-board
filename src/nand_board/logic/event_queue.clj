(ns nand-board.logic.event-queue
  (:require [clojure.spec.alpha :as s]
            [nand-board.logic.state-spec :as state-spec]))

(defn make-event-queue []
  {:post [(s/valid? ::state-spec/event-queue %)]}
  (sorted-set-by #(compare [(:time %1) (:pin-id %1)] [(:time %2) (:pin-id %2)])))

(defn add-event [event-queue event]
  {:pre  [(s/valid? ::state-spec/event-queue event-queue)
          (s/valid? ::state-spec/event event)]
   :post [(s/valid? ::state-spec/event-queue %)]}
  (-> event-queue
      (disj event)
      (conj event)))

(defn add-events [event-queue events]
  {:pre  [(s/valid? ::state-spec/event-queue event-queue)
          (every? (partial s/valid? ::state-spec/event) events)]
   :post [(s/valid? ::state-spec/event-queue %)]}
  (reduce add-event event-queue events))
