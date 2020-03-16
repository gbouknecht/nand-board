(ns nand-board.logic.state-spec
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [nand-board.logic.board-spec :as board-spec]
            [nand-board.logic.common-spec :as common-spec]))

(defn- difference [coll1 coll2]
  (set/difference (set coll1) (set coll2)))

(defn- board-pin-ids [state]
  (keys (:pins (:board state))))

(defn- vals-pin-ids [state]
  (keys (:vals state)))

(defn- event-queue-pin-ids [state]
  (map :pin-id (:event-queue state)))

(s/def ::time (s/and int? (s/nonconforming (s/or :zero zero? :positive pos?))))
(s/def ::pin-id ::common-spec/id)
(s/def ::val #{0 1})
(s/def ::vals (s/map-of ::pin-id ::val))
(s/def ::event (s/keys :req-un [::time ::pin-id ::val]))
(s/def ::event-queue (s/coll-of ::event :kind set?))
(s/def ::state (s/and (s/keys :req-un [::time ::board-spec/board ::vals ::event-queue])
                      #(empty? (difference (vals-pin-ids %) (board-pin-ids %)))
                      #(empty? (difference (event-queue-pin-ids %) (board-pin-ids %)))
                      #(->> % :event-queue (map :time) (every? (fn [t] (<= (:time %) t))))))
