(ns nand-board.ui.drawable)

(defprotocol Drawable
  (draw [this ui-state]))
