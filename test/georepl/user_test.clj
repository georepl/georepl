(ns georepl.user-test
  (:require [clojure.test :refer :all]
            [georepl.user :as user]))



(deftest nrepl-base-test
  (testing "start stop"
    (let [[server f] (user/start)]
      (is (fn? f))
      (is (not (nil? server)))
      (is (not (nil? (user/stop server)))))))


