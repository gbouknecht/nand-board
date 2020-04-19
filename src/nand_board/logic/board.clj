(ns nand-board.logic.board
  (:require [nand-board.logic.board-spec :as board-spec]
            [nand-board.logic.spec-helpers :refer [valid?]]))

(defn make-initial-board []
  {:post [(valid? ::board-spec/board %)]}
  {:gates            {}
   :pins             {}
   :wires            {}
   :next-gate-id     0
   :next-pin-id      0
   :next-wire-id     0
   :last-added-gates []
   :last-added-wires []})

(defn- make-gate [id [pin-id-0 pin-id-1 pin-id-2]]
  {:id            id
   :input-pin-ids (sorted-set pin-id-0 pin-id-1)
   :output-pin-id pin-id-2})

(defn- make-pins [ids gate-id]
  (apply merge (map (fn [id] {id {:id id :gate-id gate-id}}) ids)))

(defn- make-wire [id output-pin-id input-pin-id]
  {:id id :output-pin-id output-pin-id :input-pin-id input-pin-id})

(defn gate-for-pin [board pin]
  {:pre  [(valid? ::board-spec/board board)
          (valid? ::board-spec/pin pin)]
   :post [(valid? ::board-spec/gate %)]}
  (let [gate-id (get-in board [:pins (:id pin) :gate-id])]
    (get-in board [:gates gate-id])))

(defn last-added-gates [board]
  {:pre  [(valid? ::board-spec/board board)]
   :post [(every? (partial valid? ::board-spec/gate) %)]}
  (:last-added-gates board))

(defn pin-for-id [board id]
  {:pre  [(valid? ::board-spec/board board)]
   :post [(valid? ::board-spec/pin %)]}
  (get-in board [:pins id]))

(defn pins-for-gates [board gates]
  {:pre  [(valid? ::board-spec/board board)
          (every? (partial valid? ::board-spec/gate) gates)]
   :post [(every? (partial valid? ::board-spec/pin) %)]}
  (let [pin-ids (mapcat #(conj (:input-pin-ids %) (:output-pin-id %)) gates)]
    (map #(get-in board [:pins %]) pin-ids)))

(defn input-pins-for-gate [board gate]
  {:pre  [(valid? ::board-spec/board board)
          (valid? ::board-spec/gate gate)]
   :post [(every? (partial valid? ::board-spec/pin) %)]}
  (map #(get-in board [:pins %]) (:input-pin-ids gate)))

(defn output-pin-for-gate [board gate]
  {:pre  [(valid? ::board-spec/board board)
          (valid? ::board-spec/gate gate)]
   :post [(valid? ::board-spec/pin %)]}
  (get-in board [:pins (:output-pin-id gate)]))

(defn input-pin? [board pin]
  (let [gate (gate-for-pin board pin)]
    (contains? (:input-pin-ids gate) (:id pin))))

(defn output-pin? [board pin]
  (let [gate (gate-for-pin board pin)]
    (= (:output-pin-id gate) (:id pin))))

(defn wires-for-pin [board pin]
  {:pre  [(valid? ::board-spec/board board)
          (valid? ::board-spec/pin pin)]
   :post [(every? (partial valid? ::board-spec/wire) %)]}
  (let [wire-ids (get-in board [:pins (:id pin) :wire-ids])]
    (map #(get-in board [:wires %]) wire-ids)))

(defn unwired? [board pin]
  {:pre  [(valid? ::board-spec/board board)
          (valid? ::board-spec/pin pin)]}
  (empty? (wires-for-pin board pin)))

(defn output-pin-for-wire [board wire]
  {:pre  [(valid? ::board-spec/board board)
          (valid? ::board-spec/wire wire)]
   :post [(valid? ::board-spec/pin %)]}
  (get-in board [:pins (:output-pin-id wire)]))

(defn input-pin-for-wire [board wire]
  {:pre  [(valid? ::board-spec/board board)
          (valid? ::board-spec/wire wire)]
   :post [(valid? ::board-spec/pin %)]}
  (get-in board [:pins (:input-pin-id wire)]))

(defn last-added-wires [board]
  {:pre  [(valid? ::board-spec/board board)]
   :post [(every? (partial valid? ::board-spec/wire) %)]}
  (:last-added-wires board))

(defn- add-gate [board]
  (let [gate-id (:next-gate-id board)
        start-pin-id (:next-pin-id board)
        end-pin-id (+ start-pin-id 3)
        pin-ids (range start-pin-id end-pin-id)
        gate (make-gate gate-id pin-ids)
        pins (make-pins pin-ids gate-id)]
    (-> board
        (assoc-in [:gates gate-id] gate)
        (update :pins merge pins)
        (assoc :next-gate-id (inc gate-id))
        (assoc :next-pin-id end-pin-id)
        (update :last-added-gates conj gate))))

(defn add-gates [board n]
  {:pre  [(valid? ::board-spec/board board)]
   :post [(valid? ::board-spec/board %)]}
  (let [board (assoc board :last-added-gates [])]
    (nth (iterate add-gate board) n)))

(defn- add-wire [board output-pin input-pin]
  {:pre [(= (:output-pin-id (gate-for-pin board output-pin)) (:id output-pin))
         (contains? (:input-pin-ids (gate-for-pin board input-pin)) (:id input-pin))]}
  (let [wire-id (:next-wire-id board)
        wire (make-wire wire-id (:id output-pin) (:id input-pin))]
    (-> board
        (update-in [:pins (:id output-pin) :wire-ids] (fnil conj #{}) wire-id)
        (update-in [:pins (:id input-pin) :wire-ids] (fnil conj #{}) wire-id)
        (assoc-in [:wires wire-id] wire)
        (assoc :next-wire-id (inc wire-id))
        (update :last-added-wires conj wire))))

(defn add-wires [board [output-pin-1 input-pin-1] & pin-pairs]
  {:pre  [(valid? ::board-spec/board board)
          (every? #(= (count %) 2) pin-pairs)]
   :post [(valid? ::board-spec/board %)]}
  (let [board (assoc board :last-added-wires [])
        pin-pairs (concat [[output-pin-1 input-pin-1]] pin-pairs)]
    (reduce (partial apply add-wire) board pin-pairs)))

(defn- remove-wire-from-pin [board pin-id wire]
  (update-in board [:pins pin-id]
             (fn [pin]
               (let [wire-ids (disj (:wire-ids pin) (:id wire))]
                 (if (empty? wire-ids)
                   (dissoc pin :wire-ids)
                   (assoc pin :wire-ids wire-ids))))))

(defn- remove-wire-from-pins [board wire]
  (let [pin-ids ((juxt :output-pin-id :input-pin-id) wire)]
    (reduce #(remove-wire-from-pin %1 %2 wire) board pin-ids)))

(defn- remove-wire [board wire]
  (-> board
      (remove-wire-from-pins wire)
      (update :wires dissoc (:id wire))))

(defn remove-wires [board wires]
  {:pre  [(valid? ::board-spec/board board)
          (every? (partial valid? ::board-spec/wire) wires)
          (every? (partial contains? (:wires board)) (map :id wires))
          (or (empty? wires) (apply distinct? wires))]
   :post [(valid? ::board-spec/board %)]}
  (reduce remove-wire board wires))

(defn remove-gate [board gate]
  {:pre  [(valid? ::board-spec/board board)
          (contains? (:gates board) (:id gate))]
   :post [(valid? ::board-spec/board %)]}
  (let [pins (pins-for-gates board [gate])
        wires (set (mapcat (partial wires-for-pin board) pins))]
    (-> board
        (remove-wires wires)
        (update :gates dissoc (:id gate))
        (update :pins #(apply dissoc % (map :id pins))))))
