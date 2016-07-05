(ns georepl.gui-base-test
  (:require [clojure.test :refer :all]
            [georepl.mathlib :as math]
            [georepl.gui-base :as gui-base]))

(def tr1 [[2 3 9 :left][5 1 8 :left][4 2 7 :left][1 4 5 :left][1 3 3 :left][4 2 2 :left]])
(def tr2 [[2 3 2 :right][5 1 3 :left][4 2 5 :left][1 4 7 :right][1 3 8 :right][4 2 9 :left]])
(def tr3 [[2 3 2 :left][5 1 3 :left][4 2 5 :left][1 4 7 :right][1 3 8 :right][4 2 9 :left]])


(deftest helpers-test
  (testing "trace-length"
    (is (math/equals? 12.78759 (#'gui-base/trace-length tr1)))
    )
  (testing "button-down-time"
    (is (= 7 (#'gui-base/button-down-time tr1)))
    ))
