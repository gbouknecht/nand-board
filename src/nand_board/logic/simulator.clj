(ns nand-board.logic.simulator
  (:require [nand-board.logic.board :refer [gate-for-pin
                                            input-pin-for-wire
                                            input-pin?
                                            input-pins-for-gate
                                            output-pin-for-gate
                                            pin-for-id
                                            wires-for-pin
                                            wires-for-pin-id]]
            [nand-board.logic.board-spec :as board-spec]
            [nand-board.logic.event-queue :refer [add-event
                                                  add-events
                                                  make-event-queue]]
            [nand-board.logic.spec-helpers :refer [valid?]]
            [nand-board.logic.state-spec :as state-spec]))

(def ^:private wire-delay 1)
(def ^:private gate-delay 2)

(defn- nand [input-vals]
  (- 1 (apply * input-vals)))

(defn- propagate-output [state gate]
  (let [output-pin (output-pin-for-gate (:board state) gate)
        wires (wires-for-pin (:board state) output-pin)
        output-val (get-in state [:vals (:id output-pin)])
        make-event (fn [pin] {:time (+ (:time state) wire-delay) :pin-id (:id pin) :val output-val})
        events (map make-event (map (partial input-pin-for-wire (:board state)) wires))]
    (update state :event-queue add-events events)))

(defn- propagate-inputs [state gate]
  (let [input-pin-ids (map :id (input-pins-for-gate (:board state) gate))
        input-vals (map #(get-in state [:vals %]) input-pin-ids)]
    (if (some nil? input-vals)
      state
      (update state :event-queue add-event {:time   (+ (:time state) gate-delay)
                                            :pin-id (:id (output-pin-for-gate (:board state) gate))
                                            :val    (nand input-vals)}))))

(defn- propagate [state pin-id]
  (let [pin (pin-for-id (:board state) pin-id)
        gate (gate-for-pin (:board state) pin)
        output-pin (output-pin-for-gate (:board state) gate)]
    (if (= pin-id (:id output-pin))
      (propagate-output state gate)
      (propagate-inputs state gate))))

(defn- apply-event [state event]
  (let [state (update state :event-queue disj event)
        pin-id (:pin-id event)
        old-val (get-in state [:vals pin-id])
        new-val (:val event)]
    (if (= old-val new-val)
      state
      (-> state
          (assoc-in [:vals pin-id] new-val)
          (propagate pin-id)))))

(defn- process-events [state]
  (let [current-events (take-while (fn [event] (= (:time event) (:time state))) (:event-queue state))]
    (reduce apply-event state current-events)))

(defn pending-events? [state]
  {:pre [(valid? ::state-spec/state state)]}
  (not (empty? (:event-queue state))))

(defn make-initial-state [board]
  {:pre  [(valid? ::board-spec/board board)]
   :post [(valid? ::state-spec/state %)]}
  (let [gates (vals (:gates board))
        unwired? (fn [pin] (empty? (wires-for-pin board pin)))
        make-event (fn [pin] {:time 0 :pin-id (:id pin) :val 0})
        events (->> gates
                    (mapcat (partial input-pins-for-gate board))
                    (filter unwired?)
                    (map make-event))
        event-queue (-> (make-event-queue) (add-events events))
        state {:time 0 :board board :vals {} :event-queue event-queue}]
    (-> state
        process-events)))

(defn tick [state]
  {:pre  [(valid? ::state-spec/state state)]
   :post [(valid? ::state-spec/state %)]}
  (-> state
      (update :time inc)
      process-events))

(defn set-val [state input-pin val]
  {:pre  [(valid? ::state-spec/state state)
          (input-pin? (:board state) input-pin)
          (empty? (wires-for-pin (:board state) input-pin))]
   :post [(valid? ::state-spec/state %)]}
  (-> state
      (update :event-queue add-event {:time (:time state) :pin-id (:id input-pin) :val val})
      process-events))
