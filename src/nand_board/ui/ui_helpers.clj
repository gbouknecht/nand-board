(ns nand-board.ui.ui-helpers)

(defn translate-geometry [geometry coords]
  (into {} (for [[k v] geometry] [k (vec (map + coords v))])))
