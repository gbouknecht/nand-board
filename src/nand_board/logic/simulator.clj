(ns nand-board.logic.simulator
  (:require [clojure.spec.alpha :as s]
            [nand-board.logic.board :refer [gate-for-pin
                                            gates
                                            input-pin-for-wire
                                            input-pin?
                                            input-pins-for-gate
                                            output-pin?
                                            output-pin-for-gate
                                            pin-for-id
                                            unwired?
                                            wires-for-pin]]
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
  (let [board (:board state)
        output-pin (output-pin-for-gate board gate)
        output-val (get-in state [:vals (:id output-pin)])
        wires (wires-for-pin board output-pin)
        make-event (fn [pin] {:time (+ (:time state) wire-delay) :pin-id (:id pin) :val output-val})
        events (map make-event (map (partial input-pin-for-wire board) wires))]
    (update state :event-queue add-events events)))

(defn- propagate-inputs [state gate]
  (let [board (:board state)
        input-pins (input-pins-for-gate board gate)
        input-vals (map #(get-in state [:vals (:id %)]) input-pins)]
    (if (some nil? input-vals)
      state
      (update state :event-queue add-event {:time   (+ (:time state) gate-delay)
                                            :pin-id (:id (output-pin-for-gate board gate))
                                            :val    (nand input-vals)}))))

(defn- propagate [state pin-id]
  (let [board (:board state)
        pin (pin-for-id board pin-id)
        gate (gate-for-pin board pin)]
    (if (output-pin? board pin)
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
  (let [gates (gates board)
        make-event (fn [pin] {:time 0 :pin-id (:id pin) :val 0})
        events (->> gates
                    (mapcat (partial input-pins-for-gate board))
                    (filter (partial unwired? board))
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

(defn get-val [state pin]
  {:pre  [(valid? ::state-spec/state state)
          (valid? ::board-spec/pin pin)]
   :post [(valid? (s/nilable ::state-spec/val) %)]}
  (get-in state [:vals (:id pin)]))

(defn set-val [state input-pin val]
  {:pre  [(valid? ::state-spec/state state)
          (input-pin? (:board state) input-pin)
          (unwired? (:board state) input-pin)]
   :post [(valid? ::state-spec/state %)]}
  (-> state
      (update :event-queue add-event {:time (:time state) :pin-id (:id input-pin) :val val})
      process-events))
