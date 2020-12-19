(ns nand-board.ui.view)

(defprotocol View
  (draw [this state])
  (bounds [this] "Returns the bounds of this view as [left top width height].")
  (contains-coords? [this coords])
  (overlaps? [this that]))
