(ns nand-board.ui.ui-state
  (:require [nand-board.logic.board :refer [add-gates
                                            last-added-gates
                                            make-initial-board]]
            [nand-board.logic.simulator :refer [make-initial-state]]
            [nand-board.ui.gate-view :refer [->GateView]]))

(def ^:private max-double-click-delay-ms 500)

(defn make-initial-ui-state
  "callbacks:
   - `:single-clicked` called in case of a single click event
   - `:double-clicked` called in case of a double click event"
  [time-ms & callbacks]
  {:time-ms            time-ms
   :timed-click-events []
   :gate-views         []
   :state              (make-initial-state (make-initial-board))
   :callbacks          (apply hash-map callbacks)})

(defn- exceeds-double-click-delay? [m1 m2]
  (> (- (:time-ms m2) (:time-ms m1)) max-double-click-delay-ms))

(defn- click-count [ui-state]
  (let [[event1 event2] (:timed-click-events ui-state)]
    (cond
      (and event1 (not event2) (exceeds-double-click-delay? event1 ui-state)) 1
      (and event1 event2 (exceeds-double-click-delay? event1 event2)) 1
      (and event1 event2 (not (exceeds-double-click-delay? event1 event2))) 2
      :else 0)))

(defn- call [callback-key ui-state & args]
  (if-let [callback (-> ui-state :callbacks callback-key)]
    (apply callback ui-state args)
    ui-state))

(defn- process-click-events [ui-state]
  (let [click-count (click-count ui-state)
        click-event (-> ui-state :timed-click-events first :event)
        ui-state (update ui-state :timed-click-events subvec click-count)]
    (case click-count
      0 ui-state
      1 (call :single-clicked ui-state click-event)
      2 (call :double-clicked ui-state click-event))))

(defn update-time-ms [ui-state time-ms]
  (-> ui-state
      (assoc :time-ms time-ms)
      process-click-events))

(defn add-click-event [ui-state event]
  (-> ui-state
      (update :timed-click-events conj {:time-ms (:time-ms ui-state) :event event})
      process-click-events))

(defn gate-views [ui-state]
  (:gate-views ui-state))

(defn add-gate-view [ui-state center]
  (let [state (:state ui-state)
        board (add-gates (:board state) 1)
        [gate] (last-added-gates board)
        new-state (make-initial-state board)]
    (-> ui-state
        (update :gate-views conj (->GateView gate center))
        (assoc :state new-state))))
