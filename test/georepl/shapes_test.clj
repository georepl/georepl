(ns georepl.shapes-test
  (:require [clojure.test :refer :all]
            [georepl.mathlib :as math]
            [georepl.elements :as elements]
            [georepl.shapes :refer :all]))


(def Pnt1 (constructPoint [5.0 10.0]))
(def Pnt2 (constructPoint [0.0 0.0]))
(def Pnt3 (constructPoint [-2.0 7.0]))
(def Pnt4 (constructPoint [-1.0 -1.0]))
(def Pnt5 (constructPoint [-1.0 -1.0]))

(def Lin1 (constructLine [2.0 4.0][9.0 11.0]))
(def Lin2 (constructLine [-6.0 6.0][14.0 11.0]))
(def Lin3 (constructLine [10.0 8.0][13.0 7.0]))

(def Cir1 (constructCircle [6.0 10.0] 1.0))
(def Cir2 (constructCircle [0.0 0.0] 2.0))

(def Arc1 (constructArc [5.0 8.0] 2.0 [7.0 8.0][5.0 6.0]))

(def e1 (assoc (constructPoint [567 524]) :name "Pnt1"))
(def e2 (assoc (constructCircle [567 524] 30) :name "Cir1"))
(def e3 (assoc (constructLine [300 250][567 524]) :name "Ln1"))
(def e4 (assoc (constructLine [125 300][570 520]) :name "Ln2"))
(def e5 (assoc (constructLine [224  42][224  24]) :name "Ln3"))

;; intersections:
;;
;; Pnt1 x Arc1 = Pnt1 x Cir2 = [[5.0 10.0]]
;; Pnt2, Pnt4, Cir1, Lin3: no intersections
;; Lin1 x Lin2 = [[7.333333 9.333333]]
;; Lin1 x Arc1 = [[4.177124 6.177124][6.822876 8.822876]]
;; Lin2 x Arc1 = [[3.016201 8.254050][6.470588 9.117647]]
;; Lin2 x Cir2 = [[6.0 9.0][6.630858 9.157714]]
;; Cir2 x Arc1 = [[5.0 10.0][6.470588 9.117647]]
;;
;; e5: no intersection
;; e1 x e3 : [[567 524]]
;; e2 x e4 : [[542.648790 506.478054]]
;; e2 x e3 : [[546.063027 502.514117]]
;; e3 x e4 : [[556.690141 513.419845]]


(def drw (constructCompound [] :subtype :drawing :filename "/testfiles/temp.grl"))

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
        (is (math/equals? (:p e2)(:p-ref e2)))))

    (testing "intersect"
      (let [e2 (scale-ref e1 [50 50] 0.5)]
        (is (= :point    (:type e2)))
        (is (= 1         (:visible e2)))
        (is (math/equals? [75 75] (:p-ref e2)))
        (is (math/equals? (:p e2)(:p-ref e2))))
      (let [pts (apply concat
                  (map (partial intersect Pnt5) [Pnt1 Pnt2 Pnt3 Pnt4 Lin1 Lin2 Lin3 Cir1 Cir2 Arc1]))]
        (is (not-any? false? (map math/equals? pts [[-1 -1]]))))
      )

    (testing "between?"
      (is (between? e1 [100 100][100 100][100 100]))
      (is (false? (between? e1 [100 100][100 100][90 100])))
      (is (false? (between? e1 [90 100][100 100][90 100])))
      )

    (testing "points"
      (is (not-any? false? (map math/equals? (sort (points e1))(sort [[100 100]])))))

    (testing "name-prefix"
      (is (= "Pnt" (name-prefix e1))))

    (testing "sort-points"
      (is (= [[100 100]] (sort-points e1 [[100 100]])))
      (is (= [[100 100]] (sort-points e1 [[100 100][100 100]])))
      (is (= [[100 100]] (sort-points e1 [[100 100][47 11][47 12]])))
      (is (empty? (sort-points e1 [[10 100][47 11][47 12]]))))

    (testing "name-prefix"
      (is (= "Pnt" (name-prefix e1))))

    (testing "cut"
      (is (empty? (cut e1 [[0 0][42 42]]))))

    (testing "transform-points"
      (let [f1 identity
            f2 (partial math/vec-add [50 -30])
            f3 (partial math/mirror-point [-10 -10][10 10])]
        (is (= e1 (transform-points e1 f1)))
        (is (= (translate e1 [50 -30]) (transform-points e1 f2)))
        (is (= (constructPoint [10.0 7.0]) (transform-points (constructPoint [7.0 10.0]) f3)))))


    ;;    (testing "form"
;;      (let [e2 (constructPoint [42 43])
;;            e3 (assoc e2 :name "test")]
;;        (is (= "test" (form e3)))))
    ))


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

    (testing "intersect"
      (let [pts1 (sort (apply concat (map (partial intersect Lin1) [Pnt1 Pnt2 Pnt3 Pnt4 Lin1 Lin2 Lin3 Cir1 Cir2 Arc1])))
            pts2 (sort (apply concat (map (partial intersect Lin2) [Pnt1 Pnt2 Pnt3 Pnt4 Lin1 Lin2 Lin3 Cir1 Cir2 Arc1])))
            pts3 (sort (apply concat (map (partial intersect Lin3) [Pnt1 Pnt2 Pnt3 Pnt4 Lin1 Lin2 Lin3 Cir1 Cir2 Arc1])))]
        (is (not-any? false? (map math/equals? pts1 (sort [[7.333333 9.333333][6.822876 8.822876][4.177124 6.177124]]))))
        (is (not-any? false? (map math/equals? pts2 (sort [[-2 7][7.333333 9.333333][6.470588 9.117647][6.0 9.0][6.630858 9.157714][3.016201 8.254050]]))))
        (is (not-any? false? (map math/equals? pts3 (sort []))))
    ))

    (testing "between?"
      (is (between? e1 [100 100][100 100][200 100]))
      (is (between? e1 [200 100][100 100][200 100]))
      (is (between? e1 [150 100][120 100][180 100]))
      (is (between? e1 [100 100][100 100][90 100]))
      (is (false? (between? e1 [90 100][100 100][200 100])))
      (is (false? (between? e1 [110 100][120 100][180 100])))
      (is (false? (between? e1 [190 100][120 100][180 100]))))

    (testing "points"
      (is (not-any? false? (map math/equals? (sort (points e1))(sort [[100 100][200 100]])))))

    (testing "sort-points"
      (is (= [[100 100]] (sort-points e1 [[100 100]])))
      (is (= [[100 100][200 100]] (sort-points e1 [[200 100][100 100]])))
      (is (= [[100 100][120 100][130 100][200 100]] (sort-points e1 [[130 100][200 100][120 100][100 100]])))
      (is (= [[100 100][120 100][130 100][200 100]] (sort-points e1 [[130 100][200 100][47 11][47 12][120 100][100 100]])))
      (is (= [[110 100][120 100][130 100]] (sort-points e1 [[130 100][47 11][47 12][120 100][110 100]])))
      (is (empty? (sort-points e1 [[30 100][47 11][47 12][120 110][110 101]]))))

    (testing "name-prefix"
      (is (= "Ln" (name-prefix e1))))

    (testing "cut"
      (let [e2 (assoc (constructLine [-4 5][3 -2]) :name "Ln3")]
        (is (= [ :delete e2 ] (cut e2 [[-4 5][3 -2]])))
        (is (= [ :delete e2 :create (assoc e2 :p1 [-1 2] :p-ref [-1 2]) ] (cut e2 [[-4 5][-1 2]])))
        (is (= [ :delete e2 :create (assoc e2 :p2 [-1 2]) ] (cut e2 [[-1 2][3 -2]])))
        (is (= [ :delete e2 :create (assoc e2 :p2 [-1 2])
                 :create (constructLine [0 1] [3 -2])] (cut e2 [[-1 2][0 1]])))
        ))

    (testing "transform-points"
      (let [f1 identity
            f2 (partial math/vec-add [50 -30])
            f3 (partial math/mirror-point [-10 -10][10 10])]
        (is (= e1 (transform-points e1 f1)))
        (is (= (translate e1 [50 -30]) (transform-points e1 f2)))
        (is (= (constructLine [10.0 7.0][10.0 -30.0]) (transform-points (constructLine [7.0 10.0][-30.0 10.0]) f3)))))

;;    (testing "form"
;;      ())
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

    (testing "intersect"
      (let [pts1 (dedupe (sort (apply concat (map (partial intersect Cir1) [Pnt1 Pnt2 Pnt3 Pnt4 Lin1 Lin2 Lin3 Cir1 Cir2 Arc1]))))
            pts2 (dedupe (sort (apply concat (map (partial intersect Cir2) [Pnt1 Pnt2 Pnt3 Pnt4 Lin1 Lin2 Lin3 Cir1 Cir2 Arc1]))))]
        (is (not-any? false? (map math/equals? pts1 (sort [[5.0 10.0][6.0 9.0][6.470588 9.117647][6.6 9.2]]))))
        (is (empty? pts2))
    ))

    (testing "between?"
      (is (between? e1 [300 200][200 100][200 300]))
      (is (between? e1 [200 300][300 200][100 200]))
      (is (false? (between? e1 [310 200][200 100][200 300])))
      (is (false? (between? e1 [200 300][310 200][100 200])))
      (is (false? (between? e1 [200 300][300 200][110 200])))
      (is (false? (between? e1 [200 100][300 200][200 300]))))

    (testing "points"
      (is (empty? (points e1))))

    (testing "name-prefix"
      (is (= "Cir" (name-prefix e1))))

    (testing "sort-points"
      (is (= [[100 200]] (sort-points e1 [[100 200]])))
      (is (= [[200 300][100 200]] (sort-points e1 [[200 300][100 200]])))
      (is (= [[200 300][200 100][300 200]] (sort-points e1 [[200 300][300 200][200 100]])))
      (is (= [[200 300][100 200][200 100][300 200]] (sort-points e1 [[200 300][100 200][300 200][200 100]])))
      (is (= [] (sort-points e1 [[30 100][47 11][47 12][120 110][110 101]])))
      (let [cir0 (constructCircle [0 0] 1)]
        (is (= [[0 1][-1 0][0 -1][1 0]] (sort-points cir0 [[0 1][0 -1][1 0][-1 0]])))
        )
      )

    (testing "cut"
      (let [e2 (constructCircle [0 0] 1)]
        (is (= [ :delete e2 ] (cut e2 [])))
        (is (= [ :delete e2 ] (cut e2 [[0 -1]])))
        (is (= [ :delete e2
                 :create (constructArc [0 0] 1 [0 1][0 -1])]
               (cut e2 [[0 -1][0 1]])))
        (is (= [ :delete e2
                 :create (constructArc [0 0] 1 [0 -1][0 1])]
               (cut e2 [[0 1][0 -1]])))
     (let [cir42 (assoc (constructCircle [364 338] 108.29589) :name "Cir42")]
       (is (= [ :delete cir42
                :create (constructArc [364 338] 108.29589 [434.12095 420.5291][280.44076 269.10984])]
              (cut cir42 [[280.44076 269.10984] [434.12095 420.5291]]))))))


    (testing "transform-points"
      (let [f1 identity
            f2 (partial math/vec-add [50 -30])
            f3 (partial math/mirror-point [-10 -10][10 10])]
        (is (= e1 (transform-points e1 f1)))
        (is (= (translate e1 [50 -30]) (transform-points e1 f2)))
        (is (= (constructCircle [10.0 7.0] 42) (transform-points (constructCircle [7.0 10.0] 42) f3)))))

;;    (testing "form"
;;      ())
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

    (testing "intersect"
      (let [pts1 (dedupe (sort (apply concat (map (partial intersect Arc1) [Pnt1 Pnt2 Pnt3 Pnt4 Lin1 Lin2 Lin3 Cir1 Cir2 Arc1]))))]
        (is (not-any? false? (map math/equals? (sort pts1) (sort [[3.016201 8.254050][4.177124 6.177124][5.0 10.0][6.822876 8.822876][6.6 9.2][6.630858 9.157714]]))))
    ))

    (testing "between?"
      (is (between? e1 [200 300][300 200][200 300]))
      (is (false? (between? e1 [200 300][300 200][100 200])))
      (let [e2 (constructArc [200 200] 100 [300 200] [100 200])]
        (is (between? e2 [200 300][300 200][100 200])))
      (is (false? (between? e1 [310 200][200 100][200 300])))
      (is (false? (between? e1 [200 300][310 200][100 200])))
      (is (false? (between? e1 [200 300][300 200][110 200])))
      (is (false? (between? e1 [200 100][300 200][200 300]))))

    (testing "points"
      (is (not-any? false? (map math/equals? (sort (points e1))(sort [[300 200] [200 300]])))))

    (testing "name-prefix"
      (is (= "Arc" (name-prefix e1))))

    (testing "sort-points"
      (let [e2 (constructArc [200 200] 100 [100 200][200 300])]
        (is (= [[200 300]] (sort-points e2 [[200 300]])))
        (is (= [[300 200][100 200]] (sort-points e2 [[300 200][100 200]])))
        (is (= [[300 200][200 300][100 200]] (sort-points e2 [[100 200][200 300][300 200]])))
        (is (= [[200 300][100 200][200 100][300 200]] (sort-points e2 [[200 300][100 200][300 200][200 100]])))
        (is (empty? (sort-points e2 [[30 100][47 11][47 12][120 110][110 101]])))))

    (testing "cut"
      (let [e2 (assoc (constructArc [0 0] 1 [1 0][0 -1]) :name "Arc1")]
        (is (= [ :delete e2 ] (cut e2 [[1 0][0 -1]])))
        (is (= [ :delete e2 :create (assoc e2 :p-start [-1 0] :p-ref [-1 0]) ] (cut e2 [[1 0][-1 0]])))
        (is (= [ :delete e2 :create (assoc e2 :p-end [0 1]) ] (cut e2 [[0 1][0 -1]])))
        (is (= [ :delete e2
                 :create (assoc e2 :p-end [0 1])
                 :create (constructArc [0 0] 1 [-1 0][0 -1]) ]
               (cut e2 [[0 1][-1 0]])))
      ))

    (testing "transform-points"
      (let [f1 identity
            f2 (partial math/vec-add [50 -30])
            f3 (partial math/mirror-point [-10 -10][10 10])]
        (is (= e1 (transform-points e1 f1)))
        (is (= (translate e1 [50 -30]) (transform-points e1 f2)))
        (is (= (constructArc [10.0 7.0] 42.0 [52.0 7.0][-32.0 7.0]) (transform-points (constructArc [7.0 10.0] 42.0 [7.0 52.0][7.0 -32.0]) f3)))))

;;    (testing "form"
;;      ())
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

    (testing "between?"
      (is (false? (between? e1 [42 43][0 8][15 4711]))))

    (testing "points"
      (is (not-any? false? (map math/equals? (sort (points e0))(sort [[-400 300][-300 -100][-200 -200][-100 100][0 0][100 -100][200 100][100 200]])))))

    (testing "name-prefix"
      (nil? (name-prefix e1)))

    (testing "sort-points"
      (is (empty? (sort-points e1 [[30 100][47 11][47 12][120 110][110 101]]))))

    (testing "cut"
      (is (empty? (cut e1 [[0 0][42 42]]))))

    (testing "transform-points"
      (let [f1 identity
            f2 (partial math/vec-add [50 -30])
            f3 (partial math/mirror-point [-10 -10][10 10])]
        (is (= e1 (transform-points e1 f1)))
        (is (= (translate e1 [50 -30]) (transform-points e1 f2)))
        (is (= (constructContour [[10.0 7.0][12.0 8.0][8.0 10.0]]) (transform-points (constructContour [[7.0 10.0] [8.0 12.0][10.0 8.0]]) f3)))))

;;    (testing "form"
;;      ())
    ))


(deftest compound-test
  (let [p1 (constructPoint [150 250])
        l1 (constructLine [400 350][250 60])
        c1 (constructCircle [450 450] 80)
        a1 (constructArc [200 450] 30 [230 450][200 480])
        ct (constructContour [[100 350][50 450][150 400][200 350]])
        coll (list p1 l1 c1 a1 ct)
        e1 (constructCompound coll)]
    (testing "compound elements"
      (is (= (count (:elems e1)) 5))
      (is (= (count (:elems (constructCompound [l1 c1 a1]))) 3)))

    (testing "constructCompound"
      (is (= :compound (:type e1)))
      (is (= 0         (:visible e1)))
      (is (math/equals? [150 250] (:p-ref e1))))

    (testing "constructCompound with empty elements list"
      (let [e0 (constructCompound [])]
       (is (= :compound (:type e0)))
       (is (= 0         (:visible e0)))
       (is (empty? (:p-ref e0)))))

    (testing "constructCompound with optional key values"
      (let [e0 (constructCompound [] :anykey :foo :anyotherkey :buz)]
       (is (= :compound (:type e0)))
       (is (= :none (:subtype e0)))
       (is (= :foo (:anykey e0)))
       (is (= :buz (:anyotherkey e0)))
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

    (testing "intersect"
      (let [Drw1 (constructCompound [Lin1 Lin2 Cir1 Arc1] :subtype :drawing)
        pts1 (intersect Drw1 Drw1)]
        (is (not-any? false? (map math/equals? (sort pts1) (sort [[3.016201 8.254050][4.177124 6.177124][5.0 10.0][6.0 9.0][6.470588 9.117647][6.6 9.2][6.630858 9.157714][6.822876 8.822876][7.333333 9.333333]])))))
      (let [Drw2 (constructCompound [Lin3 Pnt2 Lin2 Cir2 Pnt1] :subtype :drawing)
            pts2 (intersect Drw2 Drw2)]
        (is (empty? pts2)))
      (let [Drw3 (constructCompound [Lin1 Lin2 Arc1] :subtype :drawing)
            pts3 (intersect Drw3 Drw3)]
        (is (not-any? false? (map math/equals? (sort pts3) (sort [[3.016201 8.254050][4.177124 6.177124][6.630858 9.157714][6.822876 8.822876][7.333333 9.333333]])))))
      (let [Drw4 (constructCompound [e1 e2 e3 e4 e5] :subtype :drawing)
            pts4 (intersect Drw4 Drw4)]
        (is (not-any? false? (map math/equals? (sort pts4) (sort [[542.648790 506.478054] [546.063027 502.514117] [556.690141 513.419845]])))))
      )

    (testing "between?"
      (is (false? (between? e1 [42 43][0 8][15 4711]))))

    (testing "points"
      (let [Drw0 (constructCompound [Pnt3 Lin2 Cir2 Pnt1 Lin1 Arc1 Cir1 Pnt4])]
        (is (not-any? false? (map math/equals? (sort (points Drw0))(sort [[-2.0 7.0][-6.0 6.0][14.0 11.0][5.0 10.0][2.0 4.0][9.0 11.0][7.0 8.0][5.0 6.0][-1.0 -1.0]]))))
        (let [Cmp1 (constructCompound [Pnt3 Lin2 Cir2])]
          (is (not-any? false? (map math/equals? (sort (points Cmp1))(sort [[-2.0 7.0][-6.0 6.0][14.0 11.0]]))))
          (let [Cmp2 (constructCompound [Cmp1 Pnt1])]
            (is (not-any? false? (map math/equals? (sort (points Cmp2))(sort [[-2.0 7.0][-6.0 6.0][14.0 11.0][5.0 10.0]]))))
            (let [Cmp3 (constructCompound [Pnt4 Cmp2 Lin1])]
              (is (not-any? false? (map math/equals? (sort (points Cmp3))(sort [[-2.0 7.0][-6.0 6.0][-1.0 -1.0][2.0 4.0][14.0 11.0][5.0 10.0][9.0 11.0]])))))))))

    (testing "name-prefix"
      (nil? (name-prefix e1)))

    (testing "sort-points"
      (is (empty? (sort-points e1 [[30 100][47 11][47 12][120 110][110 101]]))))

    (testing "cut"
      (is (empty? (cut e1 [[0 0][42 42]]))))

    (testing "transform-points"
      (let [f1 identity
            f2 (partial math/vec-add [50 -30])
            f3 (partial math/mirror-point [-10 -10][10 10])]
        (is (= e1 (transform-points e1 f1)))
        (is (= (translate e1 [50 -30]) (transform-points e1 f2)))
        (let [p2 (constructPoint [250.0 150.0])
              l2 (constructLine [350.0 400.0][60.0 250.0])
              c2 (constructCircle [450.0 450.0] 80)
              a2 (constructArc [450.0 200.0] 30 [450.0 230.0][480.0 200.0])
              cnt2 (constructContour [[350.0 100.0][450.0 50.0][400.0 150.0][350.0 200.0]])
              e2 (constructCompound [p2 l2 c2 a2 cnt2])]
                (is (= e2 (transform-points e1 f3))))))

;;    (testing "form"
;;      ())
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

    (testing "intersect"
      (let [e2 (constructText "Keep cool, calm, and collected!" [100 200][350 180])]
        (is (empty? (intersect e1 e2)))))

    (testing "between?"
      (is (false? (between? e1 [42 43][0 8][15 4711]))))

    (testing "points"
      (is (empty? (points e1))))

    (testing "name-prefix"
      (nil? (name-prefix e1)))

    (testing "sort-points"
      (is (empty? (sort-points e1 [[30 100][47 11][47 12][120 110][110 101]]))))

    (testing "cut"
      (is (empty? (cut e1 [[0 0][42 42]]))))

    (testing "transform-points"
      (let [f1 identity
            f2 (partial math/vec-add [50 -30])
            f3 (partial math/mirror-point [-10 -10][10 10])]
        (is (= e1 (transform-points e1 f1)))
        (is (= (translate e1 [50 -30]) (transform-points e1 f2)))
        (is (= (constructText "What the hack!" [100.0 100.0][80.0 350.0]) (transform-points e1 f3)))))

;;    (testing "form"
;;      ())
))

