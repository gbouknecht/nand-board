(ns nand-board.ui.ui-state
  (:require [nand-board.logic.board :refer [add-gates
                                            last-added-gates
                                            make-initial-board]]
            [nand-board.logic.simulator :refer [make-initial-state
                                                tick]]
            [nand-board.ui.gate-view :refer [->GateView]]
            [nand-board.ui.view :refer [contains-coords?
                                        overlaps?]]))

(def ^:private max-double-click-delay-ms 500)

(defn make-initial-ui-state
  "options:
   - `:time-ms`          current time in milliseconds
   - `:tick-interval-ms` interval in milliseconds between state ticks
   - `:single-clicked`   called in case of a single click event
   - `:double-clicked`   called in case of a double click event"
  [& options]
  (let [options (apply hash-map options)
        time-ms (or (:time-ms options) 0)]
    {:time-ms            time-ms
     :last-tick-time-ms  time-ms
     :tick-interval-ms   (or (:tick-interval-ms options) 1000)
     :timed-click-events []
     :gate-views         []
     :state              (make-initial-state (make-initial-board))
     :options            options}))

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
  (if-let [callback (-> ui-state :options callback-key)]
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

(defn- tick-state-at-interval [ui-state]
  (let [delta-time-ms (- (:time-ms ui-state) (:last-tick-time-ms ui-state))
        tick-count (quot delta-time-ms (:tick-interval-ms ui-state))
        ticks #(nth (iterate tick %) tick-count)]
    (-> ui-state
        (update :state ticks)
        (update :last-tick-time-ms + (* tick-count (:tick-interval-ms ui-state))))))

(defn update-time-ms [ui-state time-ms]
  (-> ui-state
      (assoc :time-ms time-ms)
      process-click-events
      tick-state-at-interval))

(defn add-click-event [ui-state event]
  (-> ui-state
      (update :timed-click-events conj {:time-ms (:time-ms ui-state) :event event})
      process-click-events))

(defn gate-views [ui-state]
  (:gate-views ui-state))

(defn add-gate-view-if-no-overlaps [ui-state center]
  (let [new-board (add-gates (-> ui-state :state :board) 1)
        new-state (make-initial-state new-board)
        [gate] (last-added-gates new-board)
        gate-view (->GateView gate center)]
    (if (some (partial overlaps? gate-view) (gate-views ui-state))
      ui-state
      (-> ui-state
          (update :gate-views conj gate-view)
          (assoc :state new-state)))))

(defn view-at-coords [ui-state coords]
  (some #(if (contains-coords? % coords) %) (gate-views ui-state)))
