(ns nand-board.logic.simulator
  (:require [nand-board.logic.board :refer [gate-for-pin-id
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
  (let [output-pin-id (:output-pin-id gate)
        wires         (wires-for-pin-id (:board state) output-pin-id)
        output-val    (get-in state [:vals output-pin-id])
        make-event    (fn [pin-id] {:time (+ (:time state) wire-delay) :pin-id pin-id :val output-val})
        events        (map make-event (map :input-pin-id wires))]
    (update state :event-queue add-events events)))

(defn- propagate-inputs [state gate]
  (let [input-vals (map #(get-in state [:vals %]) (:input-pin-ids gate))]
    (if (some nil? input-vals)
      state
      (update state :event-queue add-event {:time   (+ (:time state) gate-delay)
                                            :pin-id (:output-pin-id gate)
                                            :val    (nand input-vals)}))))

(defn- propagate [state pin-id]
  (let [gate (gate-for-pin-id (:board state) pin-id)]
    (if (= pin-id (:output-pin-id gate))
      (propagate-output state gate)
      (propagate-inputs state gate))))

(defn- apply-event [state event]
  (let [state   (update state :event-queue disj event)
        pin-id  (:pin-id event)
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
  (let [gates       (vals (:gates board))
        unwired?    (fn [pin-id] (empty? (wires-for-pin-id board pin-id)))
        make-event  (fn [pin-id] {:time 0 :pin-id pin-id :val 0})
        events      (->> gates
                         (mapcat :input-pin-ids)
                         (filter unwired?)
                         (map make-event))
        event-queue (-> (make-event-queue) (add-events events))
        state       {:time 0 :board board :vals {} :event-queue event-queue}]
    (-> state
        process-events)))

(defn tick [state]
  {:pre  [(valid? ::state-spec/state state)]
   :post [(valid? ::state-spec/state %)]}
  (-> state
      (update :time inc)
      process-events))

(defn set-val [state input-pin-id val]
  {:pre  [(valid? ::state-spec/state state)
          (some #{input-pin-id} (:input-pin-ids (gate-for-pin-id (:board state) input-pin-id)))
          (empty? (wires-for-pin-id (:board state) input-pin-id))]
   :post [(valid? ::state-spec/state %)]}
  (-> state
      (update :event-queue add-event {:time (:time state) :pin-id input-pin-id :val val})
      process-events))
