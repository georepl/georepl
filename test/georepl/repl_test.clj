(ns georepl.repl-test
  (:require [clojure.test :refer :all]
            [georepl.repl :as repl]))



;;(deftest lanterna-test
;;  (let [st (#'repl/editor (#'repl/init))]
;;    (testing "init"
;;      (is (= (dissoc st :term) {:prefix "GeoRepl=> " :history [] :j 0}))
;;      (is (not (nil? (:term st)))))
;;    (testing "output"
;;      (let [st2 (#'repl/output  st "What the hack is this?")]
;;        (is (= "What the hack is this?" (:curline st2)))
;;        (is (empty? (:curline (#'repl/output  st))))))
;;      (repl/exit st)))
