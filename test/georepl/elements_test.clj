(ns georepl.elements-test
  (:require [clojure.test :refer :all]
            [georepl.elements :refer :all]))



(deftest elems-test1
  (testing "clear, elems-length, elems-push, elems-pop"
    (clear)
    (is (= 0 (elems-length)))
    (is (= (push-elem {:type :point, :visible 1, :points [567 524], :text ["P1" 557 504 577 514]}))
        {:type :point, :visible 1, :points [567 524], :text ["P1" 557 504 577 514]})
    (is (= (push-elem {:type :point, :visible 1, :points [537 276], :text ["P2" 527 256 547 266]}))
        {:type :point, :visible 1, :points [537 276], :text ["P2" 527 256 547 266]})
    (is (= 2 (elems-length)))
    (is (= (pop-elem)
        {:type :point, :visible 1, :points [537 276], :text ["P2" 527 256 547 266]}))
    ))


(deftest elems-test2
  (testing "translate-relative"
    (push-elem {:type :circle :visible  1 :refpoint [100 100] :id 1})
    (push-elem {:type :arc    :visible -1 :refpoint [100 200] :id 2})
    (push-elem {:type :line   :visible  1 :refpoint [100 300] :id 3})
    (push-elem {:type :circle :visible -1 :refpoint [300 100] :id 4})
    (push-elem {:type :arc    :visible  1 :refpoint [300 200] :id 5})
    (push-elem {:type :line   :visible -1 :refpoint [300 300] :id 6})
    (is (= (reduce translate-relative (list-bench) [50 60])
           #{{:type :line, :visible -1, :refpoint [350 360], :id 6}
             {:type :circle, :visible -1, :refpoint [350 160], :id 4}
             {:type :arc, :visible -1, :refpoint [150 260], :id 2}}))
    ))
