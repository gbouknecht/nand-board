(ns nand-board.ui.view)

(defprotocol View
  (draw [this ui-state])
  (bounds [this] "Returns the bounds of this view as [left top width height]."))
