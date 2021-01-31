(ns nand-board.ui.ui-core
  (:require [nand-board.ui.view :refer [draw
                                        process-single-click]]
            [nand-board.ui.ui-state :refer [add-gate-view-if-no-overlaps
                                            add-click-event
                                            gate-views
                                            make-initial-ui-state
                                            update-time-ms
                                            view-at-coords]]
            [quil.core :as q]
            [quil.middleware :as m]))

(defn- time-ms []
  (System/currentTimeMillis))

(defn- coords [event]
  [(:x event) (:y event)])

(defn- single-clicked [ui-state event]
  (if-let [view (view-at-coords ui-state (coords event))]
    (update ui-state :state #(process-single-click view (coords event) %))
    ui-state))

(defn- double-clicked [ui-state event]
  (add-gate-view-if-no-overlaps ui-state (coords event)))

(defn- setup-ui-state []
  (make-initial-ui-state
    :time-ms (time-ms)
    :single-clicked single-clicked
    :double-clicked double-clicked))

(defn- update-ui-state [ui-state]
  (update-time-ms ui-state (time-ms)))

(defn- draw-ui-state [ui-state]
  (q/background 255)
  (doseq [gate-view (gate-views ui-state)]
    (draw gate-view (:state ui-state))))

(defn- mouse-clicked [ui-state event]
  (add-click-event ui-state event))

(declare nand-board)

(defn start []
  (q/defsketch
    nand-board
    :title "Nand Board"
    :size [(/ (q/screen-width) 2) (/ (q/screen-height) 2)]
    :features [:resizable]
    :setup setup-ui-state
    :update update-ui-state
    :draw draw-ui-state
    :mouse-clicked mouse-clicked
    :middleware [m/fun-mode]))
