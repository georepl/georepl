(ns georepl.shapes-factory-test
  (:require [clojure.test :refer :all]
            [georepl.mathlib :as math]
            [georepl.shapes :as shapes]
            [georepl.elements :as elements]
            [georepl.shapes-factory :as shapesfactory]))


(deftest point-factory-test
  (let [drw (elements/push-elem
              (assoc (shapes/constructCompound []) :subtype :drawing))
        e (shapes/constructPoint [100 100])
        fact (shapesfactory/createShapeFactory e)]
    (testing "current-strategy and change-strategy"
      (is (= "draw point" (:s (#'shapesfactory/current-strategy fact))))
      (is (= "draw pin point" (:s (#'shapesfactory/current-strategy (#'shapesfactory/change-strategy fact "draw pin point"))))))
    (testing "next-question"
      (let [f (get-in (#'shapesfactory/next-question fact 0) [0 :quector 0 :f])]
        (is (math/equals? [10 20] (get-in (f fact [10 20]) [:elem :p]))))
      (let [f (get-in (#'shapesfactory/next-question fact 0) [0 :quector 1 :f])]
        (is (nil? f))))
    (testing "create and current-element"
      (is (= e (shapesfactory/current-element fact)))
      (is (= false (:complete? fact))))
    (testing "refresh"
      (is (math/equals?
            [110 105]
            (:p (shapesfactory/current-element
                 (shapesfactory/refresh fact [110 105]))))))
    (testing "update-element"
      (is (and
            (math/equals?
              [60 250]
              (:p (shapesfactory/current-element
                    (shapesfactory/update-element fact (shapes/constructPoint [60 250])))))
            (math/equals?
              [60 250]
              (:p-ref (shapesfactory/current-element
                    (shapesfactory/update-element fact (shapes/constructPoint [60 250]))))))))
    (testing "current-question"
      (let [state (shapesfactory/current-question fact [42.0 17.0])]
        (is (fn? (get-in state [:strategies 0 :f])))
        (is (= "draw point" (get-in state [:strategies 0 :s])))
        (is (fn? (get-in state [:strategies 0 :quector 0 :f])))
        (is (fn? (get-in state [:strategies 0 :quector 0 :g])))
        (is (= "draw pin point" (get-in state [:strategies 1 :s])))
        (is (fn? (get-in state [:strategies 1 :quector 0 :f])))
        (is (fn? (get-in state [:strategies 1 :quector 0 :g])))))
    (testing "current-dialog"
      (is (= 2 (count (shapesfactory/current-dialog fact)))))
    (testing "finish"
      (is (= :point (first (shapesfactory/finish fact))))
      (is (= "Pnt1" (:name (#'elements/newest-shape)))))))


(deftest line-factory-test
  (let [drw (elements/push-elem
              (assoc (shapes/constructCompound []) :subtype :drawing))
        e (shapes/constructLine [20 50][100 100])
        fact (shapesfactory/createShapeFactory e)]
    (testing "current-strategy and change-strategy"
      (is (= "polygone" (:s (#'shapesfactory/current-strategy fact))))
      (is (= "orthogonal mode" (:s (#'shapesfactory/current-strategy (#'shapesfactory/change-strategy fact "orthogonal mode"))))))
    (testing "next-question"
      (let [f (get-in (#'shapesfactory/next-question fact 0) [0 :quector 0 :f])]
        (is (math/equals? [20 50] (get-in (f fact [10 20]) [:elem :p1]))))
      (let [f (get-in (#'shapesfactory/next-question fact 0) [0 :quector 1 :f])]
        (is (nil? f))))
    (testing "create and current-element"
      (is (= e (shapesfactory/current-element fact)))
      (is (= false (:complete? fact))))
    (testing "refresh"
      (let [fact-new (shapesfactory/refresh fact [110 105])
            elem-new (shapesfactory/current-element fact-new)]
        (is (math/equals? (:p-ref elem-new)(:p2 elem-new)))
        (is (math/equals? [20 50] (:p1 elem-new)))
        (is (math/equals? [110 105] (:p2 elem-new)))))
    (testing "update-element"
      (let [fact-new (shapesfactory/update-element fact (shapes/constructLine [60 250][-15 30]))
            elem-new (shapesfactory/current-element fact-new)]
      (is (math/equals? (:p-ref elem-new)(:p1 elem-new)))
      (is (math/equals? [60 250] (:p1 elem-new)))
      (is (math/equals? [-15 30] (:p2 elem-new)))))
    (testing "current-question"
      (let [state (shapesfactory/current-question fact [42.0 17.0])]
        (is (fn? (get-in state [:strategies 0 :f])))
        (is (= "polygone" (get-in state [:strategies 0 :s])))
        (is (fn? (get-in state [:strategies 0 :quector 0 :f])))
        (is (fn? (get-in state [:strategies 0 :quector 0 :g])))
        (is (= "orthogonal mode" (get-in state [:strategies 1 :s])))
        (is (fn? (get-in state [:strategies 1 :quector 0 :f])))
        (is (fn? (get-in state [:strategies 1 :quector 0 :g])))))
    (testing "current-dialog"
      (is (= 2 (count (shapesfactory/current-dialog fact)))))
    (testing "finish"
      (is (= :line (first (shapesfactory/finish fact))))
      (is (= "Ln1" (:name (#'elements/newest-shape)))))
    (testing "next-point"
      (is (= [19 22] (#'shapesfactory/next-point [19 22] e)))
      (is (= [20 22] (#'shapesfactory/next-point [19 22] (assoc e :ortho-polyline? true)))))))


(deftest circle-factory-test
  (let [drw (elements/push-elem
              (assoc (shapes/constructCompound []) :subtype :drawing))
        e (shapes/constructCircle [80 30] 50)
        fact (shapesfactory/createShapeFactory e)]
    (testing "current-strategy and change-strategy"
      (is (= "define center point & radius" (:s (#'shapesfactory/current-strategy fact))))
      (is (= "circumcircle (three points)" (:s (#'shapesfactory/current-strategy (#'shapesfactory/change-strategy fact "circumcircle (three points)"))))))
    (testing "next-question"
      (let [f (get-in (#'shapesfactory/next-question fact 0) [0 :quector 0 :f])]
        (is (math/equals? [80 30] (get-in (f fact [10 20]) [:elem :p-center])))
        (is (math/equals? 70.71068 (get-in (f fact [10 20]) [:elem :radius]))))
      (let [f (get-in (#'shapesfactory/next-question fact 0) [0 :quector 1 :f])]
        (is (math/equals? [10 20] (get-in (f fact [10 20]) [:elem :p-center])))
        (is (math/equals? 50 (get-in (f fact [10 20]) [:elem :radius])))))
    (testing "create and current-element"
      (is (= e (shapesfactory/current-element fact)))
      (is (= false (:complete? fact))))
    (testing "refresh"
      (let [fact-new1 (shapesfactory/refresh fact [100 100])
            elem-new1 (shapesfactory/current-element fact-new1)
            fact-new2 (shapesfactory/refresh fact-new1 [100 180])
            elem-new2 (shapesfactory/current-element fact-new2)]
        (is (math/equals? (:p-ref elem-new1)(:p-center elem-new1)))
        (is (math/equals? [100 100] (:p-center elem-new1)))
        (is (math/equals? 50 (:radius elem-new1)))
        (is (math/equals? (:p-ref elem-new2)(:p-center elem-new2)))
        (is (math/equals? [100 180] (:p-center elem-new2)))
        (is (math/equals? 50 (:radius elem-new2)))))
    (testing "update-element"
      (let [fact-new (shapesfactory/update-element fact (shapes/constructCircle [60 250] 30))
            elem-new (shapesfactory/current-element fact-new)]
      (is (math/equals? (:p-ref elem-new)(:p-center elem-new)))
      (is (math/equals? [60 250] (:p-center elem-new)))
      (is (math/equals? 30 (:radius elem-new)))))
    (testing "current-question"
      (let [state (shapesfactory/current-question fact [42.0 17.0])]
        (is (fn? (get-in state [:strategies 0 :f])))
        (is (= "define center point & radius" (get-in state [:strategies 0 :s])))
        (is (fn? (get-in state [:strategies 0 :quector 0 :f])))
        (is (fn? (get-in state [:strategies 0 :quector 0 :g])))
        (is (fn? (get-in state [:strategies 0 :quector 1 :f])))
        (is (fn? (get-in state [:strategies 0 :quector 1 :g])))
        (is (= "circumcircle (three points)" (get-in state [:strategies 1 :s])))
        (is (fn? (get-in state [:strategies 1 :quector 0 :f])))
        (is (fn? (get-in state [:strategies 1 :quector 0 :g])))
        (is (fn? (get-in state [:strategies 1 :quector 1 :f])))
        (is (fn? (get-in state [:strategies 1 :quector 1 :g])))
        (is (fn? (get-in state [:strategies 1 :quector 2 :f])))
        (is (fn? (get-in state [:strategies 1 :quector 2 :g])))))
    (testing "current-dialog"
      (is (= 2 (count (shapesfactory/current-dialog fact)))))
    (testing "finish"
      (is (= :circle (first (shapesfactory/finish fact))))
      (is (= "Cir1" (:name (#'elements/newest-shape)))))))


(deftest arc-factory-test
  (let [drw (elements/push-elem
              (assoc (shapes/constructCompound []) :subtype :drawing))
        e (shapes/constructArc [80 30] 50 [30 30][130 30])
        fact (shapesfactory/createShapeFactory e)]
    (testing "current-strategy and change-strategy"
      (is (= "start end on points" (:s (#'shapesfactory/current-strategy fact))))
      (is (= "next arc strat" (:s (#'shapesfactory/current-strategy (#'shapesfactory/change-strategy fact "next arc strat"))))))
    (testing "next-question"
      (let [f (get-in (#'shapesfactory/next-question fact 0) [0 :quector 0 :f])]
        (is (math/equals? [80 30] (get-in (f fact [10 20]) [:elem :p-center])))
        (is (math/equals? 50 (get-in (f fact [10 20]) [:elem :radius]))))
      (let [f (get-in (#'shapesfactory/next-question fact 0) [0 :quector 1 :f])]
        (is (math/equals? [80.0 -95.0] (get-in (f fact [10 20]) [:elem :p-center])))
        (is (math/equals? 134.62912 (get-in (f fact [10 20]) [:elem :radius])))))
    (testing "create and current-element"
      (is (= e (shapesfactory/current-element fact))))
    (testing "refresh"
      (let [fact-new1 (shapesfactory/refresh fact [100 100])
            elem-new1 (shapesfactory/current-element fact-new1)
            fact-new2 (shapesfactory/refresh fact-new1 [100 180])
            elem-new2 (shapesfactory/current-element fact-new2)]
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
      (let [fact-new (shapesfactory/update-element fact (shapes/constructArc [60 250] 30 [90 250][60 280]))
            elem-new (shapesfactory/current-element fact-new)]
      (is (math/equals? (:p-ref elem-new)(:p-start elem-new)))
      (is (math/equals? [60 250] (:p-center elem-new)))
      (is (math/equals? [90 250] (:p-start elem-new)))
      (is (math/equals? [60 280] (:p-end elem-new)))
      (is (math/equals? 30 (:radius elem-new)))))
    (testing "current-question"
      (let [state (shapesfactory/current-question fact [42.0 17.0])]
        (is (fn? (get-in state [:strategies 0 :f])))
        (is (= "start end on points" (get-in state [:strategies 0 :s])))
        (is (fn? (get-in state [:strategies 0 :quector 0 :f])))
        (is (fn? (get-in state [:strategies 0 :quector 0 :g])))
        (is (fn? (get-in state [:strategies 0 :quector 1 :f])))
        (is (fn? (get-in state [:strategies 0 :quector 1 :g])))
        (is (fn? (get-in state [:strategies 0 :quector 2 :f])))
        (is (fn? (get-in state [:strategies 0 :quector 2 :g])))
        (is (= "next arc strat" (get-in state [:strategies 1 :s])))
        (is (fn? (get-in state [:strategies 1 :quector 0 :f])))
        (is (fn? (get-in state [:strategies 1 :quector 0 :g])))
        (is (fn? (get-in state [:strategies 1 :quector 1 :f])))
        (is (fn? (get-in state [:strategies 1 :quector 1 :g])))
        (is (fn? (get-in state [:strategies 1 :quector 2 :f])))
        (is (fn? (get-in state [:strategies 1 :quector 2 :g])))))
    (testing "current-dialog"
      (is (= 2 (count (shapesfactory/current-dialog fact)))))
    (testing "finish"
      (is (= :arc (first (shapesfactory/finish fact))))
      (is (= "Arc1" (:name (#'elements/newest-shape)))))))


(deftest contour-factory-test
  (let [drw (elements/push-elem
              (assoc (shapes/constructCompound []) :subtype :drawing))
        coll [[80 30] [50 70] [30 30][130 30]]
        e (shapes/constructContour coll)
        fact (shapesfactory/createShapeFactory e)]
    (testing "current-strategy and change-strategy"
      (is (= "pick a trace" (:s (#'shapesfactory/current-strategy fact))))
      (is (= "spline" (:s (#'shapesfactory/current-strategy (#'shapesfactory/change-strategy fact "spline"))))))
    (testing "next-question"
      (let [f (get-in (#'shapesfactory/next-question fact 0) [0 :quector 0 :f])]
        (is (math/equals? [130 30] (first (get-in (f fact [10 20]) [:elem :p-list])))))
      (let [f (get-in (#'shapesfactory/next-question fact 0) [0 :quector 1 :f])]
        (is (nil? f))))
    (testing "create and current-element"
      (is (= e (shapesfactory/current-element fact)))
      (is (= false (:complete? fact))))
    (testing "refresh"
      (let [fact-new (shapesfactory/refresh fact [100 100])
            elem-new (shapesfactory/current-element fact-new)]
        (is (math/equals? (:p-ref elem-new)(first (:p-list elem-new))))
        (is (math/equals? [-80 -280] (reduce math/vec-add (map math/vec-sub coll (:p-list elem-new)))))))
    (testing "update-element"
      (let [coll [[60 250] [90 130] [90 250][60 280]]
            fact-new (shapesfactory/update-element fact (shapes/constructContour coll))
            elem-new (shapesfactory/current-element fact-new)]
        (is (math/equals? (:p-ref elem-new)(first (:p-list elem-new))))
        (is (math/equals? [60 250] (first (:p-list elem-new))))
        (is (math/vec-zero? (reduce math/vec-add (map math/vec-sub coll (:p-list elem-new)))))))
    (testing "current-question"
      (let [state (shapesfactory/current-question fact [42.0 17.0])]
        (is (fn? (get-in state [:strategies 0 :f])))
        (is (= "pick a trace" (get-in state [:strategies 0 :s])))
        (is (fn? (get-in state [:strategies 0 :quector 0 :f])))
        (is (fn? (get-in state [:strategies 0 :quector 0 :g])))
        (is (= "spline" (get-in state [:strategies 1 :s])))
        (is (fn? (get-in state [:strategies 1 :quector 0 :f])))
        (is (fn? (get-in state [:strategies 1 :quector 0 :g])))))
    (testing "current-dialog"
      (is (= 2 (count (shapesfactory/current-dialog fact)))))
    (testing "finish"
      (is (= :contour (first (shapesfactory/finish fact))))
      (is (= "Con1" (:name (#'elements/newest-shape)))))))
