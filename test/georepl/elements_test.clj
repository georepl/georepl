(ns georepl.elements-test
  (:require [clojure.test :refer :all]
            [georepl.elements :as elements]))



;;(deftest elems-test1
;;  (testing "clear, elems-length, push-elem, pop-elem"
;;    (#'elements/clear)
;;    (is (= 0 (#'elements/elems-length)))
;;    (is (= (elements/push-elem {:type :point, :visible 1, :points [567 524], :text ["P1" 557 504 577 514]}))
;;        {:type :point, :visible 1, :points [567 524], :text ["P1" 557 504 577 514]})
;;    (is (= (elements/push-elem {:type :point, :visible 1, :points [537 276], :text ["P2" 527 256 547 266]}))
;;        {:type :point, :visible 1, :points [537 276], :text ["P2" 527 256 547 266]})
;;    (is (= 2 (#'elements/elems-length)))
;;    (is (= (elements/pop-elem)
;;        {:type :point, :visible 1, :points [537 276], :text ["P2" 527 256 547 266]}))
;;    ))


;;(deftest read-file-test
;;  (testing "read-file, ... "
;;    (#'elements/read-file "test/georepl/testfiles/abstract-man.txt")))










