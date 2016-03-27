(ns georepl.shapes-test
  (:require [clojure.test :refer :all]
            [georepl.mathlib :as math]
            [georepl.shapes :refer :all]))


(deftest point-test
  (let [e1 (constructPoint [100 100])]
    (testing "constructPoint"
      (is (= :point    (:type e1)))
      (is (= 1         (:visible e1)))
      (is (math/equals? [100 100] (:p-ref e1)))
      (is (math/equals? [100 100] (:p e1))))

    (testing "next-point"
      (let [[e2 p d] (next-point e1 [200 200])]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (:p-ref e1) (:p-ref e2)))
        (is (math/equals? (:p e1) (:p e2)))
        (is (math/equals? p (:p-ref e2)))
        (is (math/equals? (* d d) (* 2.0 100 100)))))

    (testing "translate"
      (let [e2 (translate e1 [100 100])]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? [200 200] (:p-ref e2)))
        (is (math/equals? (:p e2)(:p-ref e2)))))

    (testing "rotate"
      (let [e2 (rotate e1 42)]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? [100 100] (:p-ref e2)))
        (is (math/equals? (:p e2)(:p-ref e2)))))

    (testing "rotate-ref"
      (let [e2 (rotate-ref e1 [50 50] math/PI-HALF)]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? [0 100] (:p e2)))
        (is (math/equals? (:p e2)(:p-ref e2)))))

    (testing "scale"
      (let [e2 (scale e1 42)]
        (is (= :point    (:type e2)))
        (is (= 1         (:visible e2)))
        (is (math/equals? [100 100] (:p-ref e2)))
        (is (math/equals? (:p e2)(:p-ref e2)))))

    (testing "scale-ref"
      (let [e2 (scale-ref e1 [50 50] 0.5)]
        (is (= :point    (:type e2)))
        (is (= 1         (:visible e2)))
        (is (math/equals? [75 75] (:p-ref e2)))
        (is (math/equals? (:p e2)(:p-ref e2)))))))


(deftest line-test
  (let [e1 (constructLine [100 100][200 100])]
    (testing "constructLine"
      (is (= :line    (:type e1)))
      (is (= 1         (:visible e1)))
      (is (math/equals? [100 100] (:p-ref e1)))
      (is (math/equals? [100 100] (:p1 e1)))
      (is (math/equals? [200 100] (:p2 e1))))

    (testing "next-point"
      (let [[e2 p d] (next-point e1 [50 50])]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (:p-ref e1) (:p-ref e2)))
        (is (math/equals? (:p1 e1) (:p1 e2)))
        (is (math/equals? (:p2 e1) (:p2 e2)))
        (is (math/equals? p (:p-ref e2)))
        (is (math/equals? (* d d) (* 2.0 50 50)))))

    (testing "translate"
      (let [e2 (translate e1 [100 100])]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (math/dist (:p1 e1) (:p2 e1)) (math/dist (:p1 e2) (:p2 e2))))
        (is (math/equals? (:p1 e2) (:p-ref e2)))
        (is (math/equals? [200 200] (:p1 e2)))
        (is (math/equals? [300 200] (:p2 e2)))))

    (testing "rotate"
      (let [e2 (rotate e1 math/PI-HALF)]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (math/dist (:p1 e1) (:p2 e1)) (math/dist (:p1 e2) (:p2 e2))))
        (is (math/equals? (:p1 e2) (:p-ref e2)))
        (is (math/equals? [100 100] (:p1 e2)))
        (is (math/equals? [100 200] (:p2 e2)))))

    (testing "rotate-ref"
      (let [e2 (rotate-ref e1 [-50 50] math/PI-HALF)]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (math/dist (:p1 e1) (:p2 e1)) (math/dist (:p1 e2) (:p2 e2))))
        (is (math/equals? (:p1 e2) (:p-ref e2)))
        (is (math/equals? [-100 200] (:p1 e2)))
        (is (math/equals? [-100 300] (:p2 e2)))))

    (testing "scale"
      (let [e2 (scale e1 3.0)]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (* 3.0 (math/dist (:p1 e1) (:p2 e1))) (math/dist (:p1 e2) (:p2 e2))))
        (is (math/equals? (:p1 e2) (:p-ref e2)))
        (is (math/equals? [100 100] (:p1 e2)))
        (is (math/equals? [400 100] (:p2 e2)))))

    (testing "scale-ref"
      (let [e2 (scale-ref e1 [50 50] 0.5)]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (* 0.5 (math/dist (:p1 e1) (:p2 e1))) (math/dist (:p1 e2) (:p2 e2))))
        (is (math/equals? (:p1 e2) (:p-ref e2)))
        (is (math/equals? [75 75] (:p1 e2)))
        (is (math/equals? [125 75] (:p2 e2)))))
))


(deftest circle-test
  (let [e1 (constructCircle [200 200] 100)]
    (testing "constructCircle"
      (is (= :circle   (:type e1)))
      (is (= 1         (:visible e1)))
      (is (math/equals? [200 200] (:p-ref e1)))
      (is (math/equals? [200 200] (:p-center e1)))
      (is (math/equals? 100.0 (:radius e1))))

    (testing "next-point"
      (let [[e2 p d] (next-point e1 [210 210])]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (:p-ref e1) (:p-ref e2)))
        (is (math/equals? p (:p-ref e2)))
        (is (math/equals? (* d d) (* 2.0 100))))
      (let [[e2 p d] (next-point e1 [200 50])]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (:p-ref e1) (:p-ref e2)))
        (is (math/equals? p [200 100]))
        (is (math/equals? d 50))))

    (testing "translate"
      (let [e2 (translate e1 [100 100])]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (:p-center e2) (:p-ref e2)))
        (is (math/equals? [300 300] (:p-center e2)))
        (is (math/equals? 100 (:radius e2)))))

    (testing "rotate"
      (let [e2 (rotate e1 math/PI-HALF)]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (:p-ref e1) (:p-ref e2)))
        (is (math/equals? (:p-center e1) (:p-center e2)))
        (is (math/equals? (:radius e1) (:radius e2)))))

    (testing "rotate-ref"
      (let [e2 (rotate-ref e1 [50 50] (* -1.0 math/PI-HALF))]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (:p-ref e2) (:p-center e2)))
        (is (math/equals? (:radius e1) (:radius e2)))
        (is (math/equals? [200 -100] (:p-center e2)))))

    (testing "scale"
      (let [e2 (scale e1 3.0)]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (:p-ref e1) (:p-ref e2)))
        (is (math/equals? (* 3.0 (:radius e1)) (:radius e2)))
        (is (math/equals? (:p-center e1) (:p-center e2)))))

    (testing "scale-ref"
      (let [e2 (scale-ref e1 [300 100] 2.0)]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (:p-ref e2) (:p-center e2)))
        (is (math/equals? (* 2.0 (:radius e1)) (:radius e2)))
        (is (math/equals? [100 300] (:p-center e2)))))
    ))


(deftest arc-test
  (let [e1 (constructArc [200 200] 100 [300 200] [200 300])]
    (testing "constructArc"
      (is (= :arc   (:type e1)))
      (is (= 1         (:visible e1)))
      (is (math/equals? [300 200] (:p-ref e1)))
      (is (math/equals? [200 200] (:p-center e1)))
      (is (math/equals? [300 200] (:p-start e1)))
      (is (math/equals? [200 300] (:p-end e1)))
      (is (math/equals? 100.0 (:radius e1))))

    (testing "next-point"
      (let [[e2 p d] (next-point e1 [210 210])]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (:p-ref e1) (:p-ref e2)))
        (is (math/equals? p (:p-center e2)))
        (is (math/equals? (:p-start e1) (:p-start e2)))
        (is (math/equals? (:p-end e1) (:p-end e2)))
        (is (math/equals? (* d d) (* 2.0 100))))
      (let [[e2 p d] (next-point e1 [200 50])]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (:p-ref e1) (:p-ref e2)))
        (is (math/equals? p [200 200]))
        (is (math/equals? d 150)))
      (let [[e2 p d] (next-point e1 [300 50])]
        (is (math/equals? p (:p-start e2))))
      (let [[e2 p d] (next-point e1 [200 260])]
        (is (math/equals? p (:p-end e2))))
      (let [[e2 p d] (next-point e1 [100 100])]
        (is (math/equals? p (:p-center e2))))
      (let [[e2 p d] (next-point e1 [200 350])]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (:p-ref e1) (:p-ref e2)))
        (is (math/equals? p [200 300]))
        (is (math/equals? d 50))))

    (testing "translate"
      (let [e2 (translate e1 [100 100])]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (:p-ref e2) (:p-start e2)))
        (is (math/equals? (:radius e1) (:radius e2)))
        (is (math/right-from? (:p-end e2)(:p-start e2)(:p-center e2)))
        (is (math/equals? [300 300] (:p-center e2)))
        (is (math/equals? [400 300] (:p-start e2)))
        (is (math/equals? [300 400] (:p-end e2)))))

    (testing "rotate"
      (let [e2 (rotate e1 math/PI-HALF)]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (:p-ref e2) (:p-start e2)))
        (is (math/equals? (:radius e1) (:radius e2)))
        (is (math/right-from? (:p-end e2)(:p-start e2)(:p-center e2)))
        (is (math/equals? (:p-center e1) (:p-center e2)))
        (is (math/equals? [200 300] (:p-start e2)))
        (is (math/equals? [100 200] (:p-end e2)))))

    (testing "rotate-ref"
      (let [e2 (rotate-ref e1 [300 300] math/PI)]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (:p-ref e2) (:p-start e2)))
        (is (math/equals? (:radius e1) (:radius e2)))
        (is (math/right-from? (:p-end e2)(:p-start e2)(:p-center e2)))
        (is (math/equals? [400 400] (:p-center e2)))
        (is (math/equals? [300 400] (:p-start e2)))
        (is (math/equals? [400 300] (:p-end e2)))))

    (testing "scale"
      (let [e2 (scale e1 3.0)]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (:p-ref e1) (:p-ref e2)))
        (is (math/equals? (:p-center e1) (:p-center e2)))
        (is (math/right-from? (:p-end e2)(:p-start e2)(:p-center e2)))
        (is (math/equals? (* 3.0 (:radius e1)) (:radius e2)))
        (is (math/equals? [500 200] (:p-start e2)))
        (is (math/equals? [200 500] (:p-end e2)))))

    (testing "scale-ref"
      (let [e2 (scale-ref e1 [0 -100] 0.5)]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (math/equals? (:p-ref e2) (:p-start e2)))
        (is (math/right-from? (:p-end e2)(:p-start e2)(:p-center e2)))
        (is (math/equals? (* 0.5 (:radius e1)) (:radius e2)))
        (is (math/equals? [100 50] (:p-center e2)))
        (is (math/equals? [150 50] (:p-start e2)))
        (is (math/equals? [100 100] (:p-end e2)))))
    ))



(deftest contour-test
  (let [e0 (constructContour [[-400 300][-300 -100][-200 -200][-100 100][0 0][100 -100][200 100][100 200]])]
    (testing "constructContour"
        (is (= :contour  (:type e0)))
        (is (= 1         (:visible e0)))
        (is (math/equals? [-400 300] (:p-ref e0)))
        (is (math/equals? [-600 300] (reduce math/vec-add (:p-list e0)))))

    (testing "next-point"
      (let [[e2 p d] (next-point e0 [-200 200])]
        (is (math/equals? p  [-100 100]))
        (is (math/equals? (* d d) (* 2.0 100 100))))
      (let [[e2 p d] (next-point e0 [-100 -100])]
        (is (math/equals? p [-200 -200]))
        (is (math/equals? (* d d) (* 2.0 100 100))))
      (let [[e2 p d] (next-point e0 [150 50])]
        (is (math/equals? p [200 100]))
        (is (math/equals? (* d d) (* 2.0 50 50))))
      (let [[e2 p d] (next-point e0 [100 -200])]
        (is (math/equals? p [100 -100]))
        (is (math/equals? d 100))))

    (testing "translate"
      (let [e2 (translate e0 [400 300])]
        (is (= (count (:p-list e0))(count (:p-list e2))))
        (is (math/equals? [0 600] (:p-ref e2)))
        (is (math/equals? [2600 2700] (reduce math/vec-add (:p-list e2))))))

    (testing "rotate"
      (let [e2 (rotate e0 math/PI-HALF)]
        (is (= (count (:p-list e0))(count (:p-list e2))))
        (is (math/equals? [-400 300] (:p-ref e2)))
        (is (math/equals? [-1100 5000] (reduce math/vec-add (:p-list e2))))))

    (testing "rotate-ref"
      );;TODO

    (testing "scale"
      (let [e1 (constructContour [[-3 0][-2 -4][-1 1][0 0][1 -1][2 1][1 2]])]
        (let [e2 (scale e1 2.0)]
          (is (= (count (:p-list e1))(count (:p-list e2))))
          (is (math/equals? [-3 0] (:p-ref e2)))
          (is (math/equals? [17.0 -2.0] (reduce math/vec-add (:p-list e2)))))
        (let [e3 (scale (assoc e1 :p-ref [1 2]) 2.0)]
          (is (= (count (:p-list e1))(count (:p-list e3))))
          (is (math/equals? [1 2] (:p-ref e3)))
          (is (math/equals? [-11 -16] (reduce math/vec-add (:p-list e3)))))
        (let [e4 (scale (assoc e1 :p-ref [0 0]) 2.0)]
          (is (= (count (:p-list e1))(count (:p-list e4))))
          (is (math/equals? [0 0] (:p-ref e4)))
          (is (math/equals? [-4 -2] (reduce math/vec-add (:p-list e4)))))
        (let [e5 (scale e0 3.0)]
          (is (= (count (:p-list e0))(count (:p-list e5))))
          (is (math/equals? [-400 300] (:p-ref e5)))
          (is (math/equals? [4600 -3900] (reduce math/vec-add (:p-list e5)))))
        (let [e6 (scale (assoc e0 :p-ref [-100 100]) 2.0)]
          (is (= (count (:p-list e0))(count (:p-list e6))))
          (is (math/equals? [-100 100] (:p-ref e6)))
          (is (math/equals? [-400 -200] (reduce math/vec-add (:p-list e6)))))))

    (testing "scale-ref"
      (let [e1 (constructContour [[1 -3][2 -1][3 -3]])]
        (let [e2 (scale-ref e1 [-1 1] 2.0)]
          (is (= (count (:p-list e1))(count (:p-list e2))))
          (is (every? true? (map math/equals? [[3.0 -7.0] [5.0 -3.0] [7.0 -7.0]] (:p-list e2)))))
        (let [e3 (scale-ref e1 [4 3] 0.5)]
          (is (= (count (:p-list e1))(count (:p-list e3))))
          (is (every? true? (map math/equals? [[2.5 0.0] [3.0 1.0] [3.5 0.0]] (:p-list e3)))))))
    ))


(deftest compound-test
  (let [coll (list (constructPoint [150 250])
                   (constructLine [400 350][250 60])
                   (constructCircle [450 450] 80)
                   (constructArc [200 450] 30 [230 450][200 480])
                   (constructContour [[100 350][50 450][150 400][200 350]]))
        e1 (constructCompound coll)]
    (testing "constructCompound"
      (is (= :compound (:type e1)))
      (is (= 0         (:visible e1)))
      (is (math/equals? [150 250] (:p-ref e1))))

    (testing "constructCompound with empty elements list"
      (let [e0 (constructCompound [])]
       (is (= :compound (:type e0)))
       (is (= 0         (:visible e0)))
       (is (empty? (:p-ref e0)))))

    (testing "next-point"
      (let [[e2 p d] (next-point e1 [210 210])]
        (is (= :point (:type e2)))
        (is (math/equals? [150 250] p))
        (is (math/equals? 72.11103 d))
        (is (math/equals? [150 250] (:p-ref e2)))))

    (testing "translate"
      (let [e2 (translate e1 [100 100])]
        (is (math/equals? [250 350] (:p-ref e2)))
        (is (math/equals? [250 350] (:p-ref (first (:elems e2)))))
        (is (math/equals? [500 450] (:p-ref (second (:elems e2)))))
        (is (math/equals? [200 450] (:p-ref (last (:elems e2)))))
        (is (math/equals? [330 550] (:p-ref (first (reverse (butlast (:elems e2)))))))))

    (testing "rotate"
      (let [e2 (rotate e1 (* -1.0 math/PI-HALF))]
        (is (math/equals? [150 250] (:p-ref e2)))
        (is (math/equals? [150 250] (:p-ref (first (:elems e2)))))
        (is (math/equals? [250 0] (:p-ref (second (:elems e2)))))
        (is (math/equals? [250 300] (:p-ref (last (:elems e2)))))
        (is (math/equals? [350 170] (:p-ref (first (reverse (butlast (:elems e2)))))))))

    (testing "rotate-all"
      (let [e2 (rotate-all e1 [-100 150] math/PI)]
        (is (math/equals? [-350 50] (:p-ref e2)))
        (is (math/equals? [-350 50] (:p-ref (first (:elems e2)))))
        (is (math/equals? [-600 -50] (:p-ref (second (:elems e2)))))
        (is (math/equals? [-300 -50] (:p-ref (last (:elems e2)))))
        (is (math/equals? [-430 -150] (:p-ref (first (reverse (butlast (:elems e2)))))))))

    (testing "rotate-ref"
      (let [e2 (rotate-ref e1 [250 450] math/PI-HALF)]
        (is (math/equals? [450 350] (:p-ref e2)))
        (is (math/equals? [450 350] (:p-ref (first (:elems e2)))))
        (is (math/equals? [350 600] (:p-ref (second (:elems e2)))))
        (is (math/equals? [350 300] (:p-ref (last (:elems e2)))))
        (is (math/equals? [250 430] (:p-ref (first (reverse (butlast (:elems e2)))))))))

    (testing "scale"
      (let [e2 (scale e1 3.0)]
        (is (math/equals? [150 250] (:p-ref e2)))
        (is (math/equals? [150 250] (:p-ref (first (:elems e2)))))
        (is (math/equals? [900 550] (:p-ref (second (:elems e2)))))
        (is (math/equals? [100 350] (:p-ref (last (:elems e2)))))
        (is (math/equals? [390 850] (:p-ref (first (reverse (butlast (:elems e2)))))))))

    (testing "scale-all"
      (let [e2 (scale-all e1 [250 400] 1.5)]
        (is (math/equals? [100 175] (:p-ref e2)))
        (is (math/equals? [100 175] (:p-ref (first (:elems e2)))))
        (is (math/equals? [475 325] (:p-ref (second (:elems e2)))))
        (is (math/equals? [100 350] (:p-ref (last (:elems e2)))))
        (is (math/equals? [220 475] (:p-ref (first (reverse (butlast (:elems e2)))))))))

    (testing "scale-ref"
      (let [e2 (scale-ref e1 [-50 -150] 0.5)]
        (is (math/equals? [50 50] (:p-ref e2)))
        (is (math/equals? [50 50] (:p-ref (first (:elems e2)))))
        (is (math/equals? [175 100] (:p-ref (second (:elems e2)))))
        (is (math/equals? [100 350] (:p-ref (last (:elems e2)))))
        (is (math/equals? [90 150] (:p-ref (first (reverse (butlast (:elems e2)))))))))
    ))


(deftest text-test
  (let [e1 (constructText "What the hack!" [100 100][350 80])]
    (testing "constructText"
      (is (= :text    (:type e1)))
      (is (= 1         (:visible e1)))
      (is (= "What the hack!" (:str e1)))
      (is (math/equals? [225 90] (:p-ref e1)))
      (is (math/equals? [100 100] (:top-left e1)))
      (is (math/equals? [350 80] (:bottom-right e1))))

    (testing "next-point"
      (let [[e2 p d] (next-point e1 [50 50])]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (= (:str e1)(:str e2)))
        (is (math/equals? (:p-ref e1) (:p-ref e2)))
        (is (math/equals? (:top-left e1) (:top-left e2)))
        (is (math/equals? (:bottom-right e1) (:bottom-right e2)))
        (is (math/equals? (* d d) 32225))))

    (testing "translate"
      (let [e2 (translate e1 [100 50])]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (= (:str e1)(:str e2)))
        (is (math/equals? [325 140] (:p-ref e2)))
        (is (math/equals? [200 150] (:top-left e2)))
        (is (math/equals? [450 130] (:bottom-right e2)))))

    (testing "rotate"
      (let [e2 (rotate e1 math/PI-HALF)]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (= (:str e1)(:str e2)))
        (is (math/equals? (:p-ref e1) (:p-ref e2)))
        (is (math/equals? [215.0 -35.0] (:top-left e2)))
        (is (math/equals? [235.0 215.0] (:bottom-right e2)))))

    (testing "rotate-ref"
      (let [e2 (rotate-ref e1 [-50 50] math/PI-HALF)]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (= (:str e1)(:str e2)))
        (is (math/equals? [-50 50] (:p-ref e2)))
        (is (math/equals? [-100 200] (:top-left e2)))
        (is (math/equals? [-79.99999999999997 450.0] (:bottom-right e2)))))

    (testing "scale"
      (let [e2 (scale e1 3.0)]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (= (:str e1)(:str e2)))
        (is (math/equals? (:p-ref e1) (:p-ref e2)))
        (is (math/equals? [-150.0 120.0] (:top-left e2)))
        (is (math/equals? [600.0 60.0] (:bottom-right e2)))))

    (testing "scale-ref"
      (let [e2 (scale-ref e1 [50 50] 0.5)]
        (is (= (:type e1)(:type e2)))
        (is (= (:visible e1) (:visible e2)))
        (is (= (:str e1)(:str e2)))
        (is (math/equals? [137.5 70.0] (:p-ref e2)))
        (is (math/equals? [75 75] (:top-left e2)))
        (is (math/equals? [200.0 65.0] (:bottom-right e2)))))
))
