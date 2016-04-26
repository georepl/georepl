(ns georepl.repl-test
  (:require [clojure.test :refer :all]
            [georepl.repl :as repl]))



(deftest nrepl-base-test
  (testing "start stop"
    (let [[server f] (repl/start)]
      (is (fn? f))
      (is (not (nil? server)))
      (is (not (nil? (repl/stop server)))))))


