(ns nand-board.logic.common-spec-test
  (:require [clojure.spec.alpha :as s]
            [midje.sweet :refer [=> facts]]
            [nand-board.logic.common-spec :as common-spec]))

(facts
  "id"
  (s/valid? ::common-spec/id -1) => false
  (s/valid? ::common-spec/id 0) => true
  (s/valid? ::common-spec/id 1) => true
  (s/valid? ::common-spec/id 2) => true
  (s/valid? ::common-spec/id "2") => false)
