(ns nand-board.logic.board-spec
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [nand-board.logic.common-spec :as common-spec]))

(defn- make-seqable [x]
  (if (seqable? x) x (list x)))

(defn- flatten-pairs [map first-fn second-fn]
  (letfn [(entry->pairs [e] (for [first (make-seqable (first-fn e)) second (make-seqable (second-fn e))] [first second]))]
    (set (mapcat entry->pairs map))))

(defn- same-key-id-pairs [map]
  (every? #(apply = %) (flatten-pairs map key #(:id (val %)))))

(defn- empty-or-distinct? [coll]
  (or (empty? coll) (apply distinct? coll)))

(defn- intersection [coll1 coll2]
  (set/intersection (set coll1) (set coll2)))

(defn- gates-gate-pin-id-pairs [board]
  (flatten-pairs (:gates board) key #(conj (:input-pin-ids (val %)) (:output-pin-id (val %)))))

(defn- pins-gate-pin-id-pairs [board]
  (flatten-pairs (:pins board) #(:gate-id (val %)) key))

(defn- pins-pin-wire-id-pairs [board]
  (flatten-pairs (:pins board) key #(:wire-ids (val %))))

(defn- wires-pin-wire-id-pairs [board]
  (flatten-pairs (:wires board) #((juxt :output-pin-id :input-pin-id) (val %)) key))

(defn- gates-pin-ids [board key]
  (->> (:gates board) vals (map (comp seq make-seqable key)) flatten))

(defn- wires-vals [board key]
  (->> (:wires board) vals (map key)))

(s/def ::input-pin-ids (s/coll-of ::common-spec/id :kind set? :count 2))
(s/def ::output-pin-id ::common-spec/id)
(s/def ::gate (s/and (s/keys :req-un [::common-spec/id ::input-pin-ids ::output-pin-id])
                     #(not (contains? (:input-pin-ids %) (:output-pin-id %)))))
(s/def ::gates (s/and (s/map-of ::common-spec/id ::gate)
                      #(same-key-id-pairs %)))
(s/def ::wire-ids (s/coll-of ::common-spec/id :kind set? :min-count 1))
(s/def ::gate-id ::common-spec/id)
(s/def ::pin (s/keys :req-un [::common-spec/id ::gate-id] :opt-un [::wire-ids]))
(s/def ::pins (s/and (s/map-of ::common-spec/id ::pin)
                     #(same-key-id-pairs %)))
(s/def ::input-pin-id ::common-spec/id)
(s/def ::wire (s/and (s/keys :req-un [::common-spec/id ::output-pin-id ::input-pin-id])
                     #(distinct? (:output-pin-id %) (:input-pin-id %))))
(s/def ::wires (s/and (s/map-of ::common-spec/id ::wire)
                      #(same-key-id-pairs %)))
(s/def ::board (s/and (s/keys :req-un [::gates ::pins ::wires])
                      #(= (gates-gate-pin-id-pairs %) (pins-gate-pin-id-pairs %))
                      #(= (pins-pin-wire-id-pairs %) (wires-pin-wire-id-pairs %))
                      #(empty-or-distinct? (wires-vals % :input-pin-id))
                      #(empty? (intersection (gates-pin-ids % :input-pin-ids) (wires-vals % :output-pin-id)))
                      #(empty? (intersection (gates-pin-ids % :output-pin-id) (wires-vals % :input-pin-id)))))
