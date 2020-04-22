(ns nand-board.logic.state-spec
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [nand-board.logic.board-spec :as board-spec]))

(defn- difference [coll1 coll2]
  (set/difference (set coll1) (set coll2)))

(defn- board-pins [state]
  (vals (:pins (:board state))))

(defn- vals-pins [state]
  (keys (:vals state)))

(defn- event-queue-pins [state]
  (map :pin (:event-queue state)))

(s/def ::time (s/and int? (s/nonconforming (s/or :zero zero? :positive pos?))))
(s/def ::val #{0 1})
(s/def ::vals (s/map-of ::board-spec/pin ::val))
(s/def ::event (s/keys :req-un [::time ::board-spec/pin ::val]))
(s/def ::event-queue (s/and (s/coll-of ::event :kind set?) sorted?))
(s/def ::state (s/and (s/keys :req-un [::time ::board-spec/board ::vals ::event-queue])
                      #(empty? (difference (vals-pins %) (board-pins %)))
                      #(empty? (difference (event-queue-pins %) (board-pins %)))
                      #(->> (:event-queue %) (map :time) (into []) (apply <= (:time %)))))
