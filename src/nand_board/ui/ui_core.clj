(ns nand-board.ui.ui-core
  (:require [clojure.pprint :refer [pprint]]
            [nand-board.ui.ui-state :refer [add-click-event
                                            double-click-event
                                            make-initial-ui-state
                                            process-events
                                            single-click-event
                                            update-time-ms]]
            [quil.core :as q]
            [quil.middleware :as m]))

(defn- time-ms []
  (System/currentTimeMillis))

(defn- setup-ui-state []
  (make-initial-ui-state (time-ms)))

(defn- handle-single-click-event [ui-state]
  (if-let [event (single-click-event ui-state)]
    (pprint ["single", event]))
  ui-state)

(defn- handle-double-click-event [ui-state]
  (if-let [event (double-click-event ui-state)]
    (pprint ["double", event]))
  ui-state)

(defn- update-ui-state [ui-state]
  (-> ui-state
      (update-time-ms (time-ms))
      process-events
      handle-single-click-event
      handle-double-click-event))

(defn- draw-ui-state [_]
  (q/background 255)
  (q/fill 0)
  (q/text "Hello, World!" 20 20))

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
