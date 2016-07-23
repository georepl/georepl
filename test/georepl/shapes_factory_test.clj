(ns georepl.shapes-factory-test
  (:require [clojure.test :refer :all]
            [georepl.mathlib :as math]
            [georepl.shapes :as shapes]
            [georepl.elements :as elements]
            [georepl.shapes-factory :refer :all]))


(deftest point-factory-test
  (let [drw (elements/push-elem
              (assoc (shapes/constructCompound []) :subtype :drawing))
        e (shapes/constructPoint [100 100])
        fact (createShapeFactory e)]
    (testing "create and current-element"
      (is (= e (current-element fact)))
      (is (= false (:complete? fact))))
    (testing "refresh"
      (is (math/equals?
            [110 105]
            (:p (current-element
                 (refresh fact [110 105]))))))
    (testing "update-element"
      (is (and
            (math/equals?
              [60 250]
              (:p (current-element
                    (update-element fact (shapes/constructPoint [60 250])))))
            (math/equals?
              [60 250]
              (:p-ref (current-element
                    (update-element fact (shapes/constructPoint [60 250]))))))))
    (testing "current-question"
      (is (fn? (current-question fact)))
      (is (true? (:complete? ((current-question fact) fact [25 42])))))
    (testing "finish"
      (is (= :point (first (finish fact))))
      (is (= "Pnt1" (:name (#'elements/newest-shape)))))))


(deftest line-factory-test
  (let [drw (elements/push-elem
              (assoc (shapes/constructCompound []) :subtype :drawing))
        e (shapes/constructLine [20 50][100 100])
        fact (createShapeFactory e)]
    (testing "create and current-element"
      (is (= e (current-element fact)))
      (is (= false (:complete? fact))))
    (testing "refresh"
      (let [fact-new (refresh fact [110 105])
            elem-new (current-element fact-new)]
        (is (math/equals? (:p-ref elem-new)(:p2 elem-new)))
        (is (math/equals? [20 50] (:p1 elem-new)))
        (is (math/equals? [110 105] (:p2 elem-new)))))
    (testing "update-element"
      (let [fact-new (update-element fact (shapes/constructLine [60 250][-15 30]))
            elem-new (current-element fact-new)]
      (is (math/equals? (:p-ref elem-new)(:p1 elem-new)))
      (is (math/equals? [60 250] (:p1 elem-new)))
      (is (math/equals? [-15 30] (:p2 elem-new)))))
    (testing "current-question"
      (is (fn? (current-question fact))))
    (testing "finish"
      (is (= :line (first (finish fact))))
      (is (= "Ln1" (:name (#'elements/newest-shape)))))))


(deftest circle-factory-test
  (let [drw (elements/push-elem
              (assoc (shapes/constructCompound []) :subtype :drawing))
        e (shapes/constructCircle [80 30] 50)
        fact (createShapeFactory e)]
    (testing "create and current-element"
      (is (= e (current-element fact)))
      (is (= false (:complete? fact))))
    (testing "refresh"
      (let [fact-new1 (refresh fact [100 100])
            elem-new1 (current-element fact-new1)
            fact-new2 (refresh fact-new1 [100 180])
            elem-new2 (current-element fact-new2)]
        (is (math/equals? (:p-ref elem-new1)(:p-center elem-new1)))
        (is (math/equals? [100 100] (:p-center elem-new1)))
        (is (math/equals? 50 (:radius elem-new1)))
        (is (math/equals? (:p-ref elem-new2)(:p-center elem-new2)))
        (is (math/equals? [100 180] (:p-center elem-new2)))
        (is (math/equals? 50 (:radius elem-new2)))))
    (testing "update-element"
      (let [fact-new (update-element fact (shapes/constructCircle [60 250] 30))
            elem-new (current-element fact-new)]
      (is (math/equals? (:p-ref elem-new)(:p-center elem-new)))
      (is (math/equals? [60 250] (:p-center elem-new)))
      (is (math/equals? 30 (:radius elem-new)))))
    (testing "current-question"
      (is (fn? (current-question fact))))
    (testing "finish"
      (is (= :circle (first (finish fact))))
      (is (= "Cir1" (:name (#'elements/newest-shape)))))))


(deftest arc-factory-test
  (let [drw (elements/push-elem
              (assoc (shapes/constructCompound []) :subtype :drawing))
        e (shapes/constructArc [80 30] 50 [30 30][130 30])
        fact (createShapeFactory e)]
    (testing "create and current-element"
      (is (= e (current-element fact)))
      (is (= 4 (count (:quector fact)))))
    (testing "refresh"
      (let [fact-new1 (refresh fact [100 100])
            elem-new1 (current-element fact-new1)
            fact-new2 (refresh fact-new1 [100 180])
            elem-new2 (current-element fact-new2)]
        (is (math/equals? (:p-ref elem-new1)(:p-start elem-new1)))
        (is (math/equals? [150 100] (:p-center elem-new1)))
        (is (math/equals? 50 (:radius elem-new1)))
        (is (math/equals? [100 100] (:p-start elem-new1)))
        (is (math/equals? [200 100] (:p-end elem-new1)))
        (is (math/equals? (:p-ref elem-new2)(:p-start elem-new2)))
        (is (math/equals? [150 180] (:p-center elem-new2)))
        (is (math/equals? [100 180] (:p-start elem-new2)))
        (is (math/equals? [200 180] (:p-end elem-new2)))
        (is (math/equals? 50 (:radius elem-new2)))))
    (testing "update-element"
      (let [fact-new (update-element fact (shapes/constructArc [60 250] 30 [90 250][60 280]))
            elem-new (current-element fact-new)]
      (is (math/equals? (:p-ref elem-new)(:p-start elem-new)))
      (is (math/equals? [60 250] (:p-center elem-new)))
      (is (math/equals? [90 250] (:p-start elem-new)))
      (is (math/equals? [60 280] (:p-end elem-new)))
      (is (math/equals? 30 (:radius elem-new)))))
    (testing "current-question"
      (is (fn? (current-question fact))))
    (testing "finish"
      (is (= :arc (first (finish fact))))
      (is (= "Arc1" (:name (#'elements/newest-shape)))))))


(deftest contour-factory-test
  (let [drw (elements/push-elem
              (assoc (shapes/constructCompound []) :subtype :drawing))
        coll [[80 30] [50 70] [30 30][130 30]]
        e (shapes/constructContour coll)
        fact (createShapeFactory e)]
    (testing "create and current-element"
      (is (= e (current-element fact)))
      (is (= false (:complete? fact))))
    (testing "refresh"
      (let [fact-new (refresh fact [100 100])
            elem-new (current-element fact-new)]
        (is (math/equals? (:p-ref elem-new)(first (:p-list elem-new))))
        (is (math/equals? [-80 -280] (reduce math/vec-add (map math/vec-sub coll (:p-list elem-new)))))))
    (testing "update-element"
      (let [coll [[60 250] [90 130] [90 250][60 280]]
            fact-new (update-element fact (shapes/constructContour coll))
            elem-new (current-element fact-new)]
        (is (math/equals? (:p-ref elem-new)(first (:p-list elem-new))))
        (is (math/equals? [60 250] (first (:p-list elem-new))))
        (is (math/vec-zero? (reduce math/vec-add (map math/vec-sub coll (:p-list elem-new)))))))
    (testing "current-question"
      (is (fn? (current-question fact))))
    (testing "finish"
      (is (= :contour (first (finish fact))))
      (is (= "Con1" (:name (#'elements/newest-shape)))))))
