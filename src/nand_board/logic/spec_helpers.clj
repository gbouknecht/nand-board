(ns nand-board.logic.spec-helpers
  (:require [clojure.spec.alpha :as s]))

(defn valid? [spec x]
  (or (s/valid? spec x)
      (do (s/explain spec x) false)))
