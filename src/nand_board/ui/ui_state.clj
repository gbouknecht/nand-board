(ns nand-board.ui.ui-state
  (:require [nand-board.ui.gate-view :refer [->GateView]]))

(def ^:private max-double-click-delay-ms 500)

(defn make-initial-ui-state [time-ms]
  {:time-ms time-ms
   :timed-click-events []
   :gate-views []})

(defn update-time-ms [ui-state time-ms]
  (assoc ui-state :time-ms time-ms))

(defn add-click-event [ui-state event]
  (update ui-state :timed-click-events conj {:event event :time-ms (:time-ms ui-state)}))

(defn- exceeds-double-click-delay? [m1 m2]
  (> (- (:time-ms m2) (:time-ms m1)) max-double-click-delay-ms))

(defn- click-count [ui-state]
  (let [[event1 event2] (:timed-click-events ui-state)]
    (cond
      (and event1 (not event2) (exceeds-double-click-delay? event1 ui-state)) 1
      (and event1 event2 (exceeds-double-click-delay? event1 event2)) 1
      (and event1 event2 (not (exceeds-double-click-delay? event1 event2))) 2
      :else 0)))

(defn- process-click-events [ui-state]
  (let [click-count (click-count ui-state)
        [timed-click-event] (:timed-click-events ui-state)
        ui-state (-> ui-state
                     (dissoc :single-click-event :double-click-event)
                     (assoc :timed-click-events (subvec (:timed-click-events ui-state) click-count)))]
    (case click-count
      0 ui-state
      1 (assoc ui-state :single-click-event (:event timed-click-event))
      2 (assoc ui-state :double-click-event (:event timed-click-event)))))

(defn single-click-event [ui-state]
  (:single-click-event ui-state))

(defn double-click-event [ui-state]
  (:double-click-event ui-state))

(defn process-events [ui-state]
  (-> ui-state
      process-click-events))

(defn gate-views [ui-state]
  (:gate-views ui-state))

(defn add-gate [ui-state center]
  (update ui-state :gate-views conj (->GateView center)))
