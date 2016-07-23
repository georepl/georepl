(ns georepl.gui-test
  (:require [clojure.test :refer :all]
            [georepl.gui :as gui]))



(deftest helpers-test
  (testing "snap-time-exceeded?"
    (is (false? (#'gui/snap-time-exceeded? (System/currentTimeMillis))))
    (is (false? (#'gui/snap-time-exceeded? (- (System/currentTimeMillis) 498))))
    (is (false? (#'gui/snap-time-exceeded? (- (System/currentTimeMillis) 700))))
    (is (#'gui/snap-time-exceeded? (- (System/currentTimeMillis) 1002)))
    (is (#'gui/snap-time-exceeded? (- (System/currentTimeMillis) 100000)))
    )
  (testing "short-trace"
    (is (#'gui/short-trace? 0))
    (is (#'gui/short-trace? 4))
    (is (#'gui/short-trace? 15))
    (is (not (#'gui/short-trace? 21)))
    (is (not (#'gui/short-trace? 100)))
    ))
