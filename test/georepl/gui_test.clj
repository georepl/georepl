(ns georepl.gui-test
  (:require [clojure.test :refer :all]
            [georepl.gui :as gui]))



(deftest helpers-test
  (testing "snap-time-exceeded?"
    (is (#'gui/snap-time-exceeded? 0))
    (is (#'gui/snap-time-exceeded? 998))
    (is (#'gui/snap-time-exceeded? 1000))
    (is (#'gui/snap-time-exceeded? 1002))
    (is (#'gui/snap-time-exceeded? 100000))
    )
  (testing "short-trace"
    (is (#'gui/short-trace? 0))
    (is (#'gui/short-trace? 4))
    (is (not (#'gui/short-trace? 5)))
    (is (not (#'gui/short-trace? 6)))
    (is (not (#'gui/short-trace? 10000)))
    ))
