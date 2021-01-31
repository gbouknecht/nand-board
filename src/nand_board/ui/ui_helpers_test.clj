(ns nand-board.ui.ui-helpers-test
  (:require [clojure.test :refer :all]
            [nand-board.ui.ui-helpers :refer :all]))

(deftest about-translation
  (testing
    "should translate geometry to give coordinates"
    (is (= (translate-geometry {:p1 [-100 -250] :p2 [0 50]} [200 300])
           {:p1 [100 50] :p2 [200 350]}))))
