(ns nand-board.logic.common-spec
  (:require [clojure.spec.alpha :as s]))

(s/def ::id (s/and int? (s/nonconforming (s/or :zero zero? :positive pos?))))
