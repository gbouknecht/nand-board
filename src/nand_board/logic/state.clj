(ns nand-board.logic.state
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

(defn- empty-or-distinct? [coll]
  (or (empty? coll) (apply distinct? coll)))

(defn- intersection [coll1 coll2]
  (set/intersection (set coll1) (set coll2)))

(defn- gates-gate-pin-id-pairs [state]
  (flatten-pairs (:gates state) key #(tree-seq-map-get (val %) :pin-id)))

(defn- pins-gate-pin-id-pairs [state]
  (flatten-pairs (:pins state) #(:gate-id (val %)) key))

(defn- pins-pin-wire-id-pairs [state]
  (flatten-pairs (:pins state) key #(:wire-ids (val %))))

(defn- wires-pin-wire-id-pairs [state]
  (flatten-pairs (:wires state) #((juxt :output-pin-id :input-pin-id) (val %)) key))

(defn- gates-pin-ids [state parent-key]
  (-> (:gates state) (tree-seq-map-get parent-key) (tree-seq-map-get :pin-id)))

(defn- wires-vals [state key]
  (tree-seq-map-get (:wires state) key))

(s/def ::id (s/and int? (s/nonconforming (s/or :zero zero? :positive pos?))))
(s/def ::pin-id ::id)
(s/def ::val #{0 1})
(s/def ::input-or-output (s/keys :req-un [::pin-id ::val]))
(s/def ::inputs (s/coll-of ::input-or-output :kind vector? :count 2))
(s/def ::output ::input-or-output)
(s/def ::gate (s/and (s/keys :req-un [::inputs ::output])
                     #(apply distinct? (tree-seq-map-get % :pin-id))))
(s/def ::gate-id ::id)
(s/def ::gates (s/map-of ::gate-id ::gate))
(s/def ::wire-id ::id)
(s/def ::wire-ids (s/coll-of ::wire-id :kind set? :min-count 1))
(s/def ::pin (s/keys :req-un [::gate-id] :opt-un [::wire-ids]))
(s/def ::pins (s/map-of ::pin-id ::pin))
(s/def ::output-pin-id ::pin-id)
(s/def ::input-pin-id ::pin-id)
(s/def ::wire (s/and (s/keys :req-un [::output-pin-id ::input-pin-id])
                     #(distinct? (:output-pin-id %) (:input-pin-id %))))
(s/def ::wires (s/map-of ::wire-id ::wire))
(s/def ::state (s/and (s/keys :req-un [::gates ::pins ::wires])
                      #(= (gates-gate-pin-id-pairs %) (pins-gate-pin-id-pairs %))
                      #(= (pins-pin-wire-id-pairs %) (wires-pin-wire-id-pairs %))
                      #(empty-or-distinct? (wires-vals % :input-pin-id))
                      #(empty? (intersection (gates-pin-ids % :inputs) (wires-vals % :output-pin-id)))
                      #(empty? (intersection (gates-pin-ids % :output) (wires-vals % :input-pin-id)))))
