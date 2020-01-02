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

(defn- same-key-id-pairs [map id]
  (every? #(apply = %) (flatten-pairs map key #(get (val %) id))))

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
(s/def ::state (s/and (s/keys :req-un [::gates ::pins ::wires])
                      #(= (gates-gate-pin-id-pairs %) (pins-gate-pin-id-pairs %))
                      #(= (pins-pin-wire-id-pairs %) (wires-pin-wire-id-pairs %))
                      #(empty-or-distinct? (wires-vals % :input-pin-id))
                      #(empty? (intersection (gates-pin-ids % :inputs) (wires-vals % :output-pin-id)))
                      #(empty? (intersection (gates-pin-ids % :output) (wires-vals % :input-pin-id)))))

(defn initialize []
  {:post [(s/valid? ::state %)]}
  {:gates {} :pins {} :wires {} :next-gate-id 0 :next-pin-id 0 :next-wire-id 0})

(defn- make-gate [gate-id [pin-id-0 pin-id-1 pin-id-2]]
  {:gate-id gate-id
   :inputs  [{:pin-id pin-id-0 :val 0} {:pin-id pin-id-1 :val 0}]
   :output  {:pin-id pin-id-2 :val 0}})

(defn- make-pins [gate-id pin-ids]
  (apply merge (map (fn [pin-id] {pin-id {:pin-id pin-id :gate-id gate-id}}) pin-ids)))

(defn- make-wire [wire-id output-pin-id input-pin-id]
  {:wire-id wire-id :output-pin-id output-pin-id :input-pin-id input-pin-id})

(defn add-gate [state]
  {:pre  [(s/valid? ::state state)]
   :post [(s/valid? ::state %)]}
  (let [gate-id (:next-gate-id state)
        start-pin-id (:next-pin-id state)
        end-pin-id (+ start-pin-id 3)
        pin-ids (range start-pin-id end-pin-id)
        gate (make-gate gate-id pin-ids)
        pins (make-pins gate-id pin-ids)]
    (-> state
        (assoc-in [:gates gate-id] gate)
        (update :pins merge pins)
        (assoc :next-gate-id (inc gate-id))
        (assoc :next-pin-id end-pin-id))))

(defn add-wire [state output-pin-id input-pin-id]
  {:pre  [(s/valid? ::state state)
          (some #{output-pin-id} (gates-pin-ids state :output))
          (some #{input-pin-id} (gates-pin-ids state :inputs))]
   :post [(s/valid? ::state %)]}
  (let [wire-id (:next-wire-id state)
        wire (make-wire wire-id output-pin-id input-pin-id)]
    (-> state
        (update-in [:pins output-pin-id :wire-ids] (fnil conj #{}) wire-id)
        (update-in [:pins input-pin-id :wire-ids] (fnil conj #{}) wire-id)
        (assoc-in [:wires wire-id] wire)
        (assoc :next-wire-id (inc wire-id)))))

(defn- remove-wire-from-pin [state pin-id wire-id]
  (update-in state [:pins pin-id]
             (fn [pin]
               (let [wire-ids (disj (:wire-ids pin) wire-id)]
                 (if (empty? wire-ids)
                   (dissoc pin :wire-ids)
                   (assoc pin :wire-ids wire-ids))))))

(defn- remove-wire-from-pins [state pin-ids wire-id]
  (reduce #(remove-wire-from-pin %1 %2 wire-id) state pin-ids))

(defn remove-wire [state wire-id]
  {:pre  [(s/valid? ::state state)
          (contains? (:wires state) wire-id)]
   :post [(s/valid? ::state %)]}
  (let [wire ((:wires state) wire-id)
        pin-ids ((juxt :output-pin-id :input-pin-id) wire)]
    (-> state
        (remove-wire-from-pins pin-ids wire-id)
        (update :wires dissoc wire-id))))

(defn remove-wires [state wire-ids]
  {:pre  [(s/valid? ::state state)
          (or (empty? wire-ids) (apply distinct? wire-ids))]
   :post [(s/valid? ::state %)]}
  (reduce #(remove-wire %1 %2) state wire-ids))

(defn remove-gate [state gate-id]
  {:pre  [(s/valid? ::state state)
          (contains? (:gates state) gate-id)]
   :post [(s/valid? ::state %)]}
  (let [gate ((:gates state) gate-id)
        pin-ids (tree-seq-map-get gate :pin-id)
        pins (select-keys (:pins state) pin-ids)
        wire-ids (set (mapcat :wire-ids (vals pins)))]
    (-> state
        (remove-wires wire-ids)
        (update :gates dissoc gate-id)
        (update :pins #(apply dissoc % pin-ids)))))
