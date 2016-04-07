(ns georepl.elements-test
  (:require [clojure.test :refer :all]
            [georepl.elements :as elements]))


(def drw {:type :compound :subtype :drawing :elems nil :filename "/testfiles/temp.grl"})
(def e1 {:type :point :visible 1 :p [567 524] :p-ref [567 524]})
(def e2 {:type :circle :visible 1 :p-center [567 524] :radius 30 :p-ref [567 524]})
(def e3 {:type :line :visible 1 :p1 [300 250] :p2 [567 524] :p-ref [567 524]})


(deftest push-drawing-test
  (testing "push-drawing"
    (is (= (#'elements/push-drawing drw) (:drw-elem (#'elements/tos))))
    ))


(deftest stack-operations-test
  (testing "elements-length"
    (#'elements/clear)
    (is (= 0 (#'elements/elements-length))))
  (testing "initialize with empty drawing"
    (is (= drw (#'elements/push-elem drw))))
  (testing "elements-length after push-elem"
    (is (= 1 (#'elements/elements-length)))
    (is (= e1 (#'elements/push-elem e1)))
    (is (= 2 (#'elements/elements-length)))
    (is (= e2 (#'elements/push-elem e2)))
    (is (= 3 (#'elements/elements-length))))
  (testing "tos and pop-elem"
    (let [drw (:drw-elem (#'elements/tos))]
      (is (and (= :compound (:type drw))(= :drawing (:subtype drw)))))
    (is (= e2 (#'elements/newest-shape)))
    (#'elements/clear)
    (is (nil? (#'elements/tos))))
  (testing "elements-length after clear"
    (is (= 0 (#'elements/elements-length)))))


(deftest persistance-test
  (testing "create an empty drawing"
    (#'elements/clear)
    (is (= drw (#'elements/push-drawing drw)))
    (is (= drw (:drw-elem (#'elements/tos))))
    (is (nil? (#'elements/spit-drawing))))
  (testing "create a drawing"
    (is (= e3 (#'elements/push-elem e3)))
    (is (= e2 (#'elements/push-elem e2)))
    (is (= e1 (#'elements/push-elem e1))))
  (testing "spit-drawing"
    (#'elements/spit-drawing)
    (is (= :compound (:type (#'elements/slurp-drawing "workbench/testfiles/temp.grl"))))
    (is (= 5 (#'elements/elements-length)))
    (is (= 3 (#'elements/shapes-count)))
    (is (= #{e1 e2 e3} (set (#'elements/list-elems))))))










