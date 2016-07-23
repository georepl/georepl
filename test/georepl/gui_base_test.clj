(ns georepl.gui-base-test
  (:require [clojure.test :refer :all]
            [georepl.mathlib :as math]
            [georepl.gui-base :as gui-base]))

(def tr1 [[2 3 9 :left][5 1 8 :left][4 2 7 :left][1 4 5 :left][1 3 3 :left][4 2 2 :left]])
(def tr2 [[2 3 200 :right][5 1 300 :left][4 2 500 :left][1 4 700 :right][1 3 800 :right][4 2 900 :left]])
(def tr3 [[2 3 2 :left][5 1 3 :left][4 2 5 :left][1 4 7 :right][1 3 8 :right][4 2 9 :left]])
(def tr4 [[2 3 200 :right][5 1 300 :left][4 2 500 :left][1 4 700 :right][1 3 800 :right][4 2 900 :left][4 2 1900 :left]])


(deftest helpers-test
  (testing "trace-length"
    (is (math/equals? 12.78759 (#'gui-base/trace-length tr1)))
    (is (math/equals? 12.78759 (#'gui-base/trace-length tr2)))
    (is (math/equals? 12.78759 (#'gui-base/trace-length tr3)))
    (is (math/equals? 12.78759 (#'gui-base/trace-length tr4)))
    )
  (testing "button-down-time"
    (is (false? (#'gui-base/button-down-time-exceeded? tr1)))
    (is (false? (#'gui-base/button-down-time-exceeded? tr2)))
    (is (false? (#'gui-base/button-down-time-exceeded? tr3)))
    (is (false? (#'gui-base/button-down-time-exceeded? tr4)))
    (is (true? (#'gui-base/button-down-time-exceeded? (reverse tr4))))
    ))
