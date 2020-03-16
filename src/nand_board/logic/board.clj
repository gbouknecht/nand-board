(ns nand-board.logic.board
  (:require [clojure.spec.alpha :as s]
            [nand-board.logic.board-spec :as board-spec]
            [nand-board.logic.spec-helpers :refer [valid?]]))

(defn make-initial-board []
  {:post [(valid? ::board-spec/board %)]}
  {:gates {} :pins {} :wires {} :next-gate-id 0 :next-pin-id 0 :next-wire-id 0})

(defn- make-gate [id [pin-id-0 pin-id-1 pin-id-2]]
  {:id            id
   :input-pin-ids #{pin-id-0 pin-id-1}
   :output-pin-id pin-id-2})

(defn- make-pins [ids gate-id]
  (apply merge (map (fn [id] {id {:id id :gate-id gate-id}}) ids)))

(defn- make-wire [id output-pin-id input-pin-id]
  {:id id :output-pin-id output-pin-id :input-pin-id input-pin-id})

(defn gate-for-pin-id [board pin-id]
  {:pre  [(valid? ::board-spec/board board)]
   :post [(valid? (s/nilable ::board-spec/gate) %)]}
  (let [gate-id (get-in board [:pins pin-id :gate-id])]
    (get-in board [:gates gate-id])))

(defn wires-for-pin-id [board pin-id]
  {:pre  [(valid? ::board-spec/board board)]
   :post [(every? (partial valid? ::board-spec/wire) %)]}
  (let [wire-ids (get-in board [:pins pin-id :wire-ids])]
    (map #(get-in board [:wires %]) wire-ids)))

(defn add-gate [board]
  {:pre  [(valid? ::board-spec/board board)]
   :post [(valid? ::board-spec/board %)]}
  (let [gate-id      (:next-gate-id board)
        start-pin-id (:next-pin-id board)
        end-pin-id   (+ start-pin-id 3)
        pin-ids      (range start-pin-id end-pin-id)
        gate         (make-gate gate-id pin-ids)
        pins         (make-pins pin-ids gate-id)]
    (-> board
        (assoc-in [:gates gate-id] gate)
        (update :pins merge pins)
        (assoc :next-gate-id (inc gate-id))
        (assoc :next-pin-id end-pin-id))))

(defn add-wire [board output-pin-id input-pin-id]
  {:pre  [(valid? ::board-spec/board board)
          (= output-pin-id (:output-pin-id (gate-for-pin-id board output-pin-id)))
          (some #{input-pin-id} (:input-pin-ids (gate-for-pin-id board input-pin-id)))]
   :post [(valid? ::board-spec/board %)]}
  (let [wire-id (:next-wire-id board)
        wire    (make-wire wire-id output-pin-id input-pin-id)]
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
  {:pre  [(valid? ::board-spec/board board)
          (contains? (:wires board) wire-id)]
   :post [(valid? ::board-spec/board %)]}
  (let [wire    ((:wires board) wire-id)
        pin-ids ((juxt :output-pin-id :input-pin-id) wire)]
    (-> board
        (remove-wire-from-pins pin-ids wire-id)
        (update :wires dissoc wire-id))))

(defn remove-wires [board wire-ids]
  {:pre  [(valid? ::board-spec/board board)
          (or (empty? wire-ids) (apply distinct? wire-ids))]
   :post [(valid? ::board-spec/board %)]}
  (reduce #(remove-wire %1 %2) board wire-ids))

(defn remove-gate [board gate-id]
  {:pre  [(valid? ::board-spec/board board)
          (contains? (:gates board) gate-id)]
   :post [(valid? ::board-spec/board %)]}
  (let [gate     ((:gates board) gate-id)
        pin-ids  (conj (:input-pin-ids gate) (:output-pin-id gate))
        pins     (select-keys (:pins board) pin-ids)
        wire-ids (set (mapcat :wire-ids (vals pins)))]
    (-> board
        (remove-wires wire-ids)
        (update :gates dissoc gate-id)
        (update :pins #(apply dissoc % pin-ids)))))
