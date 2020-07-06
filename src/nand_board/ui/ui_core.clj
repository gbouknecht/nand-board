(ns nand-board.ui.ui-core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn- setup []
  {})

(defn- draw [_]
  (q/background 255)
  (q/fill 0)
  (q/text "Hello, World!" 20 20))

(declare nand-board)

(defn start []
  (q/defsketch
    nand-board
    :title "Nand Board"
    :size [(/ (q/screen-width) 2) (/ (q/screen-height) 2)]
    :features [:resizable]
    :setup setup
    :draw draw
    :middleware [m/fun-mode]))
