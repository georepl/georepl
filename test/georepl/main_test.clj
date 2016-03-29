(ns georepl.main-test
  (:require [clojure.test :refer :all]
            [georepl.shapes :as shapes]
            [georepl.mathlib :as math]
            [georepl.main :as main]))


(deftest str2int-test
  (testing "convert string to integer"
    (is (nil? (#'main/str2int "")))
    (is (= 1 (#'main/str2int "1")))
    (is (= 1 (#'main/str2int "1X")))
    (is (nil? (#'main/str2int "X1")))
    (is (= 42 (#'main/str2int "42")))
    ))

(deftest new-unique-filename-test
  (testing "create unique file name"
    (is (= "test0.grl" (#'main/new-unique-filename [])))
    (is (= "test2.grl" (#'main/new-unique-filename ["test.grl" "test1.grl" "test3.grl" "test0.grl" "testit.grl" "testtesttest.grl" ""])))
    (is (= "test4.grl" (#'main/new-unique-filename ["test3.grl" "test1.grl" "test5.grl" "test0.grl" "test2.grl"])))
    ))
