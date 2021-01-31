(ns nand-board.logic.simulator
  (:require [clojure.spec.alpha :as s]
            [nand-board.logic.board :refer [gate-for-pin
                                            gateless-pins
                                            gates
                                            input-pin-for-wire
                                            input-pin?
                                            input-pins-for-gate
                                            output-pin?
                                            output-pin-for-gate
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

(defn- propagate-output [state output-pin]
  (let [board (:board state)
        output-val (get-in state [:vals output-pin])
        wires (wires-for-pin board output-pin)
        make-event (fn [pin] {:time (+ (:time state) wire-delay) :pin pin :val output-val})
        events (map make-event (map (partial input-pin-for-wire board) wires))]
    (update state :event-queue add-events events)))

(defn- propagate-input [state input-pin]
  (let [board (:board state)
        gate (gate-for-pin board input-pin)
        input-pins (input-pins-for-gate board gate)
        input-vals (map #(get-in state [:vals %]) input-pins)]
    (if (some nil? input-vals)
      state
      (update state :event-queue add-event {:time (+ (:time state) gate-delay)
                                            :pin  (output-pin-for-gate board gate)
                                            :val  (nand input-vals)}))))

(defn- propagate [state pin]
  (if (output-pin? (:board state) pin)
    (propagate-output state pin)
    (propagate-input state pin)))

(defn- apply-event [state event]
  (let [state (update state :event-queue disj event)
        pin (:pin event)
        old-val (get-in state [:vals pin])
        new-val (:val event)]
    (if (= old-val new-val)
      state
      (-> state
          (assoc-in [:vals pin] new-val)
          (propagate pin)))))

(defn- process-events [state]
  (let [current-events (take-while (fn [event] (= (:time event) (:time state))) (:event-queue state))]
    (reduce apply-event state current-events)))

(defn pending-events? [state]
  {:pre [(valid? ::state-spec/state state)]}
  (not (empty? (:event-queue state))))

(defn make-initial-state [board]
  {:pre  [(valid? ::board-spec/board board)]
   :post [(valid? ::state-spec/state %)]}
  (let [make-event (fn [pin] {:time 0 :pin pin :val 0})
        gateless-pins-events (->> (gateless-pins board)
                                  (map make-event))
        gate-pins-events (->> (gates board)
                              (mapcat (partial input-pins-for-gate board))
                              (filter (partial unwired? board))
                              (map make-event))
        event-queue (-> (make-event-queue)
                        (add-events gateless-pins-events)
                        (add-events gate-pins-events))
        state {:time 0 :board board :vals {} :event-queue event-queue}]
    (-> state
        process-events)))

(defn tick [state]
  {:pre  [(valid? ::state-spec/state state)]
   :post [(valid? ::state-spec/state %)]}
  (-> state
      (update :time inc)
      process-events))

(defn ticks [state]
  (if (pending-events? state)
    (lazy-seq (cons state (ticks (tick state))))
    [state]))

(defn get-val [state pin]
  {:pre  [(valid? ::state-spec/state state)
          (valid? ::board-spec/pin pin)]
   :post [(valid? (s/nilable ::state-spec/val) %)]}
  (get-in state [:vals pin]))

(defn set-val [state pin val]
  {:pre  [(valid? ::state-spec/state state)
          (let [board (:board state)]
            (or (and (input-pin? board pin) (unwired? board pin))
                (some #{pin} (gateless-pins board))))]
   :post [(valid? ::state-spec/state %)]}
  (-> state
      (update :event-queue add-event {:time (:time state) :pin pin :val val})
      process-events))

(defn flip-val [state pin]
  (set-val state pin (- 1 (get-val state pin))))
