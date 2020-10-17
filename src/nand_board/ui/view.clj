(ns nand-board.ui.view)

(defprotocol View
  (draw [this ui-state]))
