(ns georepl.paint-test
  (:require [clojure.test :refer :all]
            [georepl.paint :refer :all]))



(deftest init-paint-test
  (testing "init-frame"
    (is (nil? (init-frame)))))
