(ns nand-board.ui.ui-core
  (:require [nand-board.ui.drawable :refer [draw]]
            [nand-board.ui.ui-state :refer [add-gate-view
                                            add-click-event
                                            gate-views
                                            make-initial-ui-state
                                            update-time-ms]]
            [quil.core :as q]
            [quil.middleware :as m]))

(defn- time-ms []
  (System/currentTimeMillis))

(defn- double-clicked [ui-state event]
  (add-gate-view ui-state [(:x event) (:y event)]))

(defn- setup-ui-state []
  (make-initial-ui-state
    :time-ms (time-ms)
    :double-clicked double-clicked))

(defn- update-ui-state [ui-state]
  (update-time-ms ui-state (time-ms)))

(defn- draw-ui-state [ui-state]
  (q/background 255)
  (doseq [gate-view (gate-views ui-state)]
    (draw gate-view ui-state)))

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
