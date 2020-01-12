(ns nand-board.logic.board
  (:require [clojure.spec.alpha :as s]
            [nand-board.logic.board-spec :as board-spec]))

(defn make-initial-board []
  {:post [(s/valid? ::board-spec/board %)]}
  {:gates {} :pins {} :wires {} :next-gate-id 0 :next-pin-id 0 :next-wire-id 0})

(defn- make-gate [gate-id [pin-id-0 pin-id-1 pin-id-2]]
  {:gate-id gate-id
   :inputs  [{:pin-id pin-id-0} {:pin-id pin-id-1}]
   :output  {:pin-id pin-id-2}})

(defn- make-pins [gate-id pin-ids]
  (apply merge (map (fn [pin-id] {pin-id {:pin-id pin-id :gate-id gate-id}}) pin-ids)))

(defn- make-wire [wire-id output-pin-id input-pin-id]
  {:wire-id wire-id :output-pin-id output-pin-id :input-pin-id input-pin-id})

(defn add-gate [board]
  {:pre  [(s/valid? ::board-spec/board board)]
   :post [(s/valid? ::board-spec/board %)]}
  (let [gate-id (:next-gate-id board)
        start-pin-id (:next-pin-id board)
        end-pin-id (+ start-pin-id 3)
        pin-ids (range start-pin-id end-pin-id)
        gate (make-gate gate-id pin-ids)
        pins (make-pins gate-id pin-ids)]
    (-> board
        (assoc-in [:gates gate-id] gate)
        (update :pins merge pins)
        (assoc :next-gate-id (inc gate-id))
        (assoc :next-pin-id end-pin-id))))

(defn- gate-for-pin-id [board pin-id]
  ((:gates board) (get-in board [:pins pin-id :gate-id])))

(defn- gate-input-pin-ids [gate]
  (map :pin-id (:inputs gate)))

(defn- gate-output-pin-id [gate]
  (:pin-id (:output gate)))

(defn add-wire [board output-pin-id input-pin-id]
  {:pre  [(s/valid? ::board-spec/board board)
          (= output-pin-id (-> board (gate-for-pin-id output-pin-id) gate-output-pin-id))
          (some #{input-pin-id} (-> board (gate-for-pin-id input-pin-id) gate-input-pin-ids))]
   :post [(s/valid? ::board-spec/board %)]}
  (let [wire-id (:next-wire-id board)
        wire (make-wire wire-id output-pin-id input-pin-id)]
    (-> board
        (update-in [:pins output-pin-id :wire-ids] (fnil conj #{}) wire-id)
        (update-in [:pins input-pin-id :wire-ids] (fnil conj #{}) wire-id)
        (assoc-in [:wires wire-id] wire)
        (assoc :next-wire-id (inc wire-id)))))

(defn- remove-wire-from-pin [board pin-id wire-id]
  (update-in board [:pins pin-id]
             (fn [pin]
               (let [wire-ids (disj (:wire-ids pin) wire-id)]
                 (if (empty? wire-ids)
                   (dissoc pin :wire-ids)
                   (assoc pin :wire-ids wire-ids))))))

(defn- remove-wire-from-pins [board pin-ids wire-id]
  (reduce #(remove-wire-from-pin %1 %2 wire-id) board pin-ids))

(defn remove-wire [board wire-id]
  {:pre  [(s/valid? ::board-spec/board board)
          (contains? (:wires board) wire-id)]
   :post [(s/valid? ::board-spec/board %)]}
  (let [wire ((:wires board) wire-id)
        pin-ids ((juxt :output-pin-id :input-pin-id) wire)]
    (-> board
        (remove-wire-from-pins pin-ids wire-id)
        (update :wires dissoc wire-id))))

(defn remove-wires [board wire-ids]
  {:pre  [(s/valid? ::board-spec/board board)
          (or (empty? wire-ids) (apply distinct? wire-ids))]
   :post [(s/valid? ::board-spec/board %)]}
  (reduce #(remove-wire %1 %2) board wire-ids))

(defn remove-gate [board gate-id]
  {:pre  [(s/valid? ::board-spec/board board)
          (contains? (:gates board) gate-id)]
   :post [(s/valid? ::board-spec/board %)]}
  (let [gate ((:gates board) gate-id)
        pin-ids (conj (gate-input-pin-ids gate) (gate-output-pin-id gate))
        pins (select-keys (:pins board) pin-ids)
        wire-ids (set (mapcat :wire-ids (vals pins)))]
    (-> board
        (remove-wires wire-ids)
        (update :gates dissoc gate-id)
        (update :pins #(apply dissoc % pin-ids)))))
