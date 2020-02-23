(ns nand-board.logic.event-queue
  (:require [nand-board.logic.spec-helpers :refer [valid?]]
            [nand-board.logic.state-spec :as state-spec]))

(defn make-event-queue []
  {:post [(valid? ::state-spec/event-queue %)]}
  (sorted-set-by #(compare [(:time %1) (:pin-id %1)] [(:time %2) (:pin-id %2)])))

(defn add-event [event-queue event]
  {:pre  [(valid? ::state-spec/event-queue event-queue)
          (valid? ::state-spec/event event)]
   :post [(valid? ::state-spec/event-queue %)]}
  (-> event-queue
      (disj event)
      (conj event)))

(defn add-events [event-queue events]
  {:pre  [(valid? ::state-spec/event-queue event-queue)
          (every? (partial valid? ::state-spec/event) events)]
   :post [(valid? ::state-spec/event-queue %)]}
  (reduce add-event event-queue events))
