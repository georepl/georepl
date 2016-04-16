(ns georepl.elements-test
  (:require [clojure.test :refer :all]
            [georepl.elements :as elements]
            [georepl.repl :as repl]))


(def drw {:type :compound :subtype :drawing :elems [] :filename "/testfiles/temp.grl"})
(def e1 {:type :point :visible 1 :p [567 524] :p-ref [567 524] :name "P1"})
(def e2 {:type :circle :visible 1 :p-center [567 524] :radius 30 :p-ref [567 524]})
(def e3 {:type :line :visible 1 :p1 [300 250] :p2 [567 524] :p-ref [567 524]})
(def e4 {:type :line :visible 1 :p1 [125 300] :p2 [570 520] :p-ref [570 520]})
(def e5 {:type :line :visible 1 :p1 [224  42] :p2 [224  24] :p-ref [224  24]})


(deftest push-drawing-test
  (testing "push-drawing"
    (is (= (#'elements/push-drawing drw nil) (:drw-elem (#'elements/tos))))
    (is (= (#'elements/push-elem e1) (#'elements/newest-shape)))
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
    (is (= drw (#'elements/push-drawing drw nil)))
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
    (elements/push-elem drw)
    (elements/push-elem (assoc e1 :name (elements/unique-name "P")))
    (is (= "P1" (elements/unique-name "P")))
    (elements/push-elem (assoc e2 :name (elements/unique-name "C")))
    (elements/push-elem (assoc e3 :name (elements/unique-name "L")))
    (is (= "L2" (elements/unique-name "L")))
    (elements/push-elem (assoc e4 :name (elements/unique-name "L")))
    (elements/push-elem (assoc e5 :name (elements/unique-name "L")))
    (is (= "L4" (elements/unique-name "L")))
    ))


(deftest curform-test
  (testing "curform on empty stack"
    (#'elements/clear)
    (is (nil? (elements/curform))))
  (testing "curform on empty drawing"
    (#'elements/push-drawing drw nil)
    (is (nil? (elements/curform))))
  (testing "curform after pushing a shape"
    (#'elements/push-elem e1)
    (is (= (format "(def P1 %s)" (pr-str e1)) (elements/curform))))
  (testing "curform after pushing a shape and reading from repl"
    (#'elements/push-elem e2)
;;    (is (= (pr-str e2) (elements/curform)))
;;    (is (= (pr-str e2) (#'repl/on-change {})))
;;    (is (nil? (elements/curform)))
    ))




