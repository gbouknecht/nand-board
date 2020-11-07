(ns nand-board.logic.state-spec-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [nand-board.logic.board :refer [add-gates
                                            last-added-gates
                                            make-initial-board
                                            pins-for-gates]]
            [nand-board.logic.state-spec :as state-spec]))

(defn- make-event-queue []
  (letfn [(time-pin-id [event] [(:time event) (:id (:pin event))])]
    (sorted-set-by #(compare (time-pin-id %1) (time-pin-id %2)))))

(deftest about-state-data-structure

  (is (not (s/valid? ::state-spec/time -1)))
  (is (s/valid? ::state-spec/time 0))
  (is (not (s/valid? ::state-spec/time 0.5)))
  (is (s/valid? ::state-spec/time 1))
  (is (s/valid? ::state-spec/time 2))
  (is (not (s/valid? ::state-spec/time "2")))

  (is (not (s/valid? ::state-spec/val -1)))
  (is (s/valid? ::state-spec/val 0))
  (is (s/valid? ::state-spec/val 1))
  (is (not (s/valid? ::state-spec/val 2)))

  (let [board (-> (make-initial-board) (add-gates 2))
        [i1 i2 o3 i4 i5 o6] (pins-for-gates board (last-added-gates board))
        state {:time        3
               :board       board
               :vals        {i1 0, o3 1, i4 0, i5 0, o6 1}
               :event-queue (conj (make-event-queue)
                                  {:time 3 :pin o3 :val 1}
                                  {:time 4 :pin i1 :val 1}
                                  {:time 4 :pin i2 :val 1}
                                  {:time 5 :pin i5 :val 1}
                                  {:time 9 :pin o3 :val 0})}]
    (is (s/valid? ::state-spec/state {:time 0 :board (make-initial-board) :vals {} :event-queue (make-event-queue)}))
    (is (s/valid? ::state-spec/state state))
    (is (not (s/valid? ::state-spec/state (update state :event-queue conj {:time 2 :pin i1 :val 1}))))))
