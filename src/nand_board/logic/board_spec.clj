(ns nand-board.logic.board-spec
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]))

(defn- tree-seq-map-get [root key]
  (->> (tree-seq seqable? seq root)
       (filter map?)
       (map #(get % key))
       (remove nil?)))

(defn- flatten-pairs [map first-fn second-fn]
  (letfn [(seq [x] (if (seqable? x) x (list x)))
          (entry->pairs [e] (for [first (seq (first-fn e)) second (seq (second-fn e))] [first second]))]
    (set (mapcat entry->pairs map))))

(defn- same-key-id-pairs [map id]
  (every? #(apply = %) (flatten-pairs map key #(get (val %) id))))

(defn- empty-or-distinct? [coll]
  (or (empty? coll) (apply distinct? coll)))

(defn- intersection [coll1 coll2]
  (set/intersection (set coll1) (set coll2)))

(defn- gates-gate-pin-id-pairs [board]
  (flatten-pairs (:gates board) key #(tree-seq-map-get (val %) :pin-id)))

(defn- pins-gate-pin-id-pairs [board]
  (flatten-pairs (:pins board) #(:gate-id (val %)) key))

(defn- pins-pin-wire-id-pairs [board]
  (flatten-pairs (:pins board) key #(:wire-ids (val %))))

(defn- wires-pin-wire-id-pairs [board]
  (flatten-pairs (:wires board) #((juxt :output-pin-id :input-pin-id) (val %)) key))

(defn- gates-pin-ids [board parent-key]
  (-> (:gates board) (tree-seq-map-get parent-key) (tree-seq-map-get :pin-id)))

(defn- wires-vals [board key]
  (tree-seq-map-get (:wires board) key))

(s/def ::id (s/and int? (s/nonconforming (s/or :zero zero? :positive pos?))))
(s/def ::pin-id ::id)
(s/def ::val #{0 1})
(s/def ::input-or-output (s/keys :req-un [::pin-id ::val]))
(s/def ::inputs (s/coll-of ::input-or-output :kind vector? :count 2))
(s/def ::output ::input-or-output)
(s/def ::gate-id ::id)
(s/def ::gate (s/and (s/keys :req-un [::gate-id ::inputs ::output])
                     #(apply distinct? (tree-seq-map-get % :pin-id))))
(s/def ::gates (s/and (s/map-of ::gate-id ::gate)
                      #(same-key-id-pairs % :gate-id)))
(s/def ::wire-id ::id)
(s/def ::wire-ids (s/coll-of ::wire-id :kind set? :min-count 1))
(s/def ::pin (s/keys :req-un [::pin-id ::gate-id] :opt-un [::wire-ids]))
(s/def ::pins (s/and (s/map-of ::pin-id ::pin)
                     #(same-key-id-pairs % :pin-id)))
(s/def ::output-pin-id ::pin-id)
(s/def ::input-pin-id ::pin-id)
(s/def ::wire (s/and (s/keys :req-un [::wire-id ::output-pin-id ::input-pin-id])
                     #(distinct? (:output-pin-id %) (:input-pin-id %))))
(s/def ::wires (s/and (s/map-of ::wire-id ::wire)
                      #(same-key-id-pairs % :wire-id)))
(s/def ::board (s/and (s/keys :req-un [::gates ::pins ::wires])
                      #(= (gates-gate-pin-id-pairs %) (pins-gate-pin-id-pairs %))
                      #(= (pins-pin-wire-id-pairs %) (wires-pin-wire-id-pairs %))
                      #(empty-or-distinct? (wires-vals % :input-pin-id))
                      #(empty? (intersection (gates-pin-ids % :inputs) (wires-vals % :output-pin-id)))
                      #(empty? (intersection (gates-pin-ids % :output) (wires-vals % :input-pin-id)))))
