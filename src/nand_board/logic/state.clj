(ns nand-board.logic.state
  (:require [clojure.spec.alpha :as s]
            [nand-board.logic.state-spec :as state-spec]))

(defn make-initial-state []
  {:post [(s/valid? ::state-spec/state %)]}
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
  {:pre  [(s/valid? ::state-spec/state state)]
   :post [(s/valid? ::state-spec/state %)]}
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

(defn- gate-for-pin-id [state pin-id]
  ((:gates state) (get-in state [:pins pin-id :gate-id])))

(defn- gate-input-pin-ids [gate]
  (map :pin-id (:inputs gate)))

(defn- gate-output-pin-id [gate]
  (:pin-id (:output gate)))

(defn add-wire [state output-pin-id input-pin-id]
  {:pre  [(s/valid? ::state-spec/state state)
          (= output-pin-id (-> state (gate-for-pin-id output-pin-id) gate-output-pin-id))
          (some #{input-pin-id} (-> state (gate-for-pin-id input-pin-id) gate-input-pin-ids))]
   :post [(s/valid? ::state-spec/state %)]}
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
  {:pre  [(s/valid? ::state-spec/state state)
          (contains? (:wires state) wire-id)]
   :post [(s/valid? ::state-spec/state %)]}
  (let [wire ((:wires state) wire-id)
        pin-ids ((juxt :output-pin-id :input-pin-id) wire)]
    (-> state
        (remove-wire-from-pins pin-ids wire-id)
        (update :wires dissoc wire-id))))

(defn remove-wires [state wire-ids]
  {:pre  [(s/valid? ::state-spec/state state)
          (or (empty? wire-ids) (apply distinct? wire-ids))]
   :post [(s/valid? ::state-spec/state %)]}
  (reduce #(remove-wire %1 %2) state wire-ids))

(defn remove-gate [state gate-id]
  {:pre  [(s/valid? ::state-spec/state state)
          (contains? (:gates state) gate-id)]
   :post [(s/valid? ::state-spec/state %)]}
  (let [gate ((:gates state) gate-id)
        pin-ids (conj (gate-input-pin-ids gate) (gate-output-pin-id gate))
        pins (select-keys (:pins state) pin-ids)
        wire-ids (set (mapcat :wire-ids (vals pins)))]
    (-> state
        (remove-wires wire-ids)
        (update :gates dissoc gate-id)
        (update :pins #(apply dissoc % pin-ids)))))

(defn get-val [state pin-id]
  {:pre  [(s/valid? ::state-spec/state state)
          (contains? (:pins state) pin-id)]
   :post [(s/valid? ::state-spec/val %)]}
  (let [gate (gate-for-pin-id state pin-id)
        get-val #(if (= (:pin-id %) pin-id) (:val %))]
    (some get-val (conj (:inputs gate) (:output gate)))))

(defn set-val [state pin-id val]
  {:pre  [(s/valid? ::state-spec/state state)
          (contains? (:pins state) pin-id)
          (s/valid? ::state-spec/val val)]
   :post [(s/valid? ::state-spec/state %)]}
  (let [gate-id (get-in state [:pins pin-id :gate-id])
        set-val #(if (= (:pin-id %) pin-id) (assoc % :val val) %)
        set-vals (comp vec (partial map set-val))]
    (update-in state [:gates gate-id]
               (fn [gate]
                 (-> gate
                     (update :output set-val)
                     (update :inputs set-vals))))))
