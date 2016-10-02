(ns georepl.mathlib-test
  (:require [clojure.test :refer :all]
            [georepl.mathlib :refer :all]))

(deftest wrappers
  (is (= 42.6 (abs 42.6)))
  (is (= 42.6 (abs -42.6)))
  (is (= 42 (round 42.3)))
  (is (= 42 (round 41.6)))
  (is (= 25 (sq 5)))
  (is (= 3.0 (sqrt 9)))
  (is (equals? 1.570796 (acos 0)))
  (is (equals? 0.0 (asin 0)))
  (is (equals? 0.0 (cos 1.570796)))
  (is (equals? 1.0 (sin 1.570796)))
  (is (= 1 (sgn 42)))
  (is (= -1 (sgn -42))))

(deftest coordinates-test
  (is (equals? [42 43](coordinates [42 43 323253252 :left]))))

(deftest nearly-zero-test
  (testing "nearly-zero?"
    (is (true? (nearly-zero? 0)))
    (is (true? (nearly-zero? -0)))
    (is (true? (nearly-zero? 0.0)))
    (is (true? (nearly-zero? -0.0)))
    (is (false? (nearly-zero? -1.0)))
    (is (false? (nearly-zero? (first (drop 16 (iterate (partial * 0.5) 1))))))
    (is (true? (nearly-zero? (first (drop 17 (iterate (partial * 0.5) 1))))))))

(deftest equals-test
  (testing "equals? on scalar values"
    (is (true? (equals? 0 0.0)))
    (is (true? (equals? 0.0 0)))
    (is (true? (equals? 5 5.0)))
    (is (false? (equals? 2.0 5.0)))
    (is (true? (equals? 12.369317054748535 12.36931687685298)))
    (is (true? (equals? 42.0 42.0))))
  (testing "equals? on collections"
    (is (true? (equals? [0 0 0] [0.0 0.0 0.0] [0.0 0.0 0] [0.0 0 0.0])))
    (is (true? (equals? [0.0 0.0] [0 0])))
    (is (true? (equals? [5 2 5.0][5 2 5.0])))
    (is (false? (equals? '(2.0 5.0) '(2.1 5.0))))
    (is (true? (equals? [0 12.369317054748535] [0 12.369316876852980])))
    (is (true? (equals? [42.0 9] [42 9.0])))))

(deftest vector-arithmetics-test
  (let [p [-2.1 5.3]
        q [3 1]
        v [1.5 1.5]
        w [0 1]]

    (testing "basics"
      (is (= [-117 -391] (vec-sub [17 29][134 420])))
      (is (= [0 124] (vec-sub [367 129][367 5])))
      (is (= [-222 -12] (vec-sub [-222 -29][0 -17])))
      (is (= [-44 -42] (vec-sub [0 0][44 42])))
      (is (= [17 29] (vec-sub [17 29][0 0])))
      (is (= [0 0] (vec-sub [0 0][0 0])))
      (is (= [8.64 -11.52]
             (vec-scal-mult
               2.4
               (vec-sub
                 (vec-add q w)
                 (vec-add p v))))))

    (testing "zero, equality and non-equality"
      (is (vec-zero? [0 0 0]))
      (is (not (vec-zero? [0 1 0])))
      (is (not-equals? p q))
      (is (not-equals? v w))
      (is (not (not-equals? p p)))
      (is (not (not-equals? w w))))

    (testing "vec-scale"
      (let [p-ref1 p
            p-ref2 q
            p-ref3 (vec-scal-mult
                     0.5
                     (vec-add p q))]
        (is (equals? p (vec-scale p-ref1 p 2.0)))
        (is (equals? q (vec-scale p-ref2 q 2.0)))
        (is (equals? [-4.65 7.45] (vec-scale p-ref3 p 2.0)))
        (is (equals? [5.55 -1.15] (vec-scale p-ref3 q 2.0)))))


    (testing "orthogonal vectors"
      (is (equals? [-1.5 1.5] (vec-ortho v)))
      (is (equals? [-1.0 0.0] (vec-ortho w))))

    (testing "length and dist"
      (is (equals? 7 (dist [16] [9])))
      (is (equals? 10.0 (length [6 8])))
      (is (equals? 325.0 (length [0 325])))
      (is (equals? 18.0 (length [18 0])))
      (is (equals? 10.0 (length [-6 8])))
      (is (equals? 18.0 (length [-18 0])))
      (is (equals? 10.0 (length [6 -8])))
      (is (equals? 325.0 (length [0 -325])))
      (is (equals? 10.0 (length [-6 -8])))
      (is (equals? 0.0 (length [0 0])))
      (is (equals? 2.1213203435596424 (length v)))
      (is (equals? 1 (length w)))
      (is (equals? 6.670832032063167 (dist p q))))

    (testing "dot-product"
      (is (equals? 0.0 (dot-product [1 0][0 1])))
      (is (equals? 1.0 (dot-product [1 0][1 0]))))

    (testing "angle"
      (is (nearly-zero? (angle [0 1][0 1])))
      (is (equals? (* -1 PI-HALF) (angle [1 0][0 1])))
      (is (equals? PI-HALF (angle [0 1][1 0])))
      (is (nearly-zero? (angle [0 0][1 1])))
      (is (equals? (/ PI 4) (angle [42 42])))
      (is (nearly-zero? (angle [0 1][0 0])))
      (is (nearly-zero? (angle [574 0])))
      (is (nearly-zero? (angle [0 0][0 0])))
      (is (nearly-zero? (angle [0 0][1 0])))
      (is (nearly-zero? (angle [0 0][-1 0])))
      (is (nearly-zero? (angle [-1 42][-1 42])))
      (is (nearly-zero? (angle [8 -49 -1079] [8 -49 -1079])))
      )))


(deftest project-line-test
  (testing "zero-length-line"
    (let [p1 [100 100]
          p2 p1]
      (is (equals? p1 (project-line [20 90] p1 p2)))))
  (testing "almost-zero-length-line"
    (let [p1 [100 100]
          p2 [100 (+ 100 (* EPS EPS))]]
      (is (equals? p1 (project-line [20 90] p1 p2)))))
  (testing "standard situations orthogonal to x/y axes"
    (let [p1 [100 100]
          p2 [100 200]]
      (is (equals? [100 90] (project-line [150 90] p1 p2)))))
  (testing "standard situations non-orthogonal to x/y"
    (let [p1 [10 10]
          p2 [20 30]]
      (is (equals? [18 26] (project-line [30 20] p1 p2))))
    (let [p1 [0 0]
          p2 [50 50]]
      (is (equals? [25 25] (project-line [30 20] p1 p2))))
    ))

(deftest mirror-point-test
  (let [p [100 100]]
    (testing "p = p1"
      (is (equals? p (mirror-point [100 100] [300 400] p))))
    (testing "p = p2"
      (is (equals? p (mirror-point [0 0] [100 100] p))))
    (testing "p on p1, p2"
      (is (equals? p (mirror-point [0 0] [300 300] p))))
    (testing "p1, p2 parallel to x-axis"
      (is (equals? [100 -100] (mirror-point [0 0] [300 0] p))))
    (testing "p1, p2 parallel to y-axis"
      (is (equals? [-100 100] (mirror-point [0 0] [0 300] p))))
    (testing "mirror axis diagonal"
      (is (equals? [200 10] (mirror-point [0 0] [300 300] [10 200]))))
      ))

(deftest on-straight-line?-test
  (testing "on-straight-line?"
    (is (on-straight-line? [55.0 55.0][0.0 0.0][100.0 100.0]))
    (is (on-straight-line? [55.0 0.0][55.0 -100.0][55.0 100.0]))
    (is (on-straight-line? [0.0 55.0][-100.0 55.0][100.0 55.0]))
    (is (not (on-straight-line? [32.0 23.0][0.0 0.0][100.0 100.0])))
    (is (not (on-straight-line? [32.0 23.0][55.0 -100.0][55.0 100.0])))
    (is (not (on-straight-line? [32.0 23.0][-100.0 55.0][100.0 55.0])))
  ))

(deftest on-line?-test
  (testing "on-line?"
    (is (on-line? [55.0 55.0][0.0 0.0][100.0 100.0]))
    (is (not (on-line? [155.0 155.0][0.0 0.0][100.0 100.0])))
    (is (on-line? [55.0 0.0][55.0 -100.0][55.0 100.0]))
    (is (not (on-line? [55.0 -100.1][55.0 -100.0][55.0 100.0])))
    (is (not (on-line? [55.0 100.1][55.0 -100.0][55.0 100.0])))
    (is (on-line? [0.0 55.0][-100.0 55.0][100.0 55.0]))
    (is (not (on-line? [-100.1 55.0][-100.0 55.0][100.0 55.0])))
    (is (not (on-line? [100.1 55.0][-100.0 55.0][100.0 55.0])))
    (is (not (on-line? [32.0 23.0][0.0 0.0][100.0 100.0])))
    (is (not (on-line? [32.0 23.0][55.0 -100.0][55.0 100.0])))
    (is (not (on-line? [32.0 23.0][-100.0 55.0][100.0 55.0])))
  ))

(deftest vec-rotate-center-test
  (let [p [0.0 5]
        q [-1.0 1]]
  (testing "vec-rotate-center"
    (is (equals? [-5.0 0.0] (vec-rotate-center p PI-HALF)))
    (is (equals? [0.0 -5.0] (vec-rotate-center p PI)))
    (is (equals? [-1.0 -1.0] (vec-rotate-center q PI-HALF)))
    (is (equals? [1.0 -1.0] (vec-rotate-center q PI))))))

(deftest vec-rotate-test
  (let [p [0.0 5]
        q [-1.0 1]
        ct [2 -2]]
  (testing "vec-rotate"
    (is (equals? [-5.0 -4.0] (vec-rotate p ct PI-HALF)))
    (is (equals? [4.0 -9.0] (vec-rotate p ct PI)))
    (is (equals? [-1.0 -5.0] (vec-rotate q ct PI-HALF)))
    (is (equals? [5.0 -5.0] (vec-rotate q ct PI))))))

(deftest project-circle-test
  (testing "project-circle"
    (is (= [100.0 50.0] (project-circle [100 10] [100 100] 50)))))

(deftest det-test
  (testing "det"
    (is (= 0 (det [0 0][0 0])))
    (is (= 0 (det [1 -2][1 -2])))
    (is (= 1 (det [3 -11][-1 4])))
    (is (= 10 (det [5 0][-1 2])))
    (is (= -450 (det [3 -42][-11 4])))
  ))

(deftest right-from?-test
  (testing "right-from?"
    (is (right-from? [[0.0 0.0][42.0 42.0]] [22.0 1.0]))
    ))

(deftest degree-test
  (testing "degree"
    (is (equals? 0.0 (degree 0.0)))
    (is (equals? 360.0 (degree TWO-PI)))
    (is (equals? 180.0 (degree PI)))
    (is (equals? 270.0 (degree (* (/ 3 2) PI))))
    ))

(deftest project-point-onto-circle-test
  (testing "project-point-onto-circle"
    (is (equals? 0.0 (length
                        (vec-sub
                         (project-point-onto-circle [203.2 203.2] [200 200] 100)
                         (project-point-onto-circle [400 400] [200 200] 100)))))
    ))

(deftest on-circle?-test
  (testing "on-circle?-test"
    (is (on-circle? [20.0 10.0][20.0 20.0] 10.0))
    (is (on-circle? [20.0 30.0][20.0 20.0] 10.0))
    (is (on-circle? [10.0 20.0][20.0 20.0] 10.0))
    (is (not (on-circle? [10.0 30.0][20.0 20.0] 10.0)))
    (is (not (on-circle? [20.0 20.0][20.0 20.0] 10.0)))
    ))

(deftest on-arc?-test
  (testing "on-arc?-test"
    (is (on-arc? [20.0 10.0][20.0 20.0] 10.0 [10.0 20.0][30.0 20.0]))
    (is (false? (on-arc? [20.0 10.0][20.0 20.0] 10.0 [10.0 20.0][31.0 21.0])))
    (is (false? (on-arc? [20.0 10.0][20.0 20.0] 10.0 [30.0 20.0][10.0 20.0])))
    ))

(deftest intersect-circles-test
  (testing "intersect-circles-test"
    (is (= [](intersect-circles [0 1] 2 [0 1] 2)))
    (is (= [](intersect-circles [0 1] 2 [0 1] 3)))
    (is (= [](intersect-circles [0 1] 2 [0 1] 2)))
    (is (= [](intersect-circles [0 0] 2 [0 1] 0)))
    (is (= [](intersect-circles [6 10] 1 [0 0] 2)))
    (let [[p q](intersect-circles [0 1] 2 [0 -1] 2)]
      (is (and
        (equals? p [-1.732051 0.0])
        (equals? q [1.732051 0.0]))))
    (let [[p q] (intersect-circles [50 50] 100 [80 60] 80)]
      (is (and
        (equals? p [140.702534 7.892397])
        (equals? q [97.297466 138.107603]))))))

(deftest intersect-circle-arc-test
  (testing "intersect-circle-arc-test"
    (let [[p q] (intersect-circles [5 5] 3 [7 4] 2)]
      (is (and
        (equals? p [6.105573 2.211146])
        (equals? q [7.894427 5.788854]))))
    (is (= 2 (count (intersect-circle-arc [5 5] 3 [7 4] 2 [5 4][7 6]))))
    (is (= 1 (count (intersect-circle-arc [5 5] 3 [7 4] 2 [5 4][7 2]))))
    (is (= 1 (count (intersect-circle-arc [5 5] 3 [7 4] 2 [7 6][7 2]))))
    (is (= 1 (count (intersect-circle-arc [5 5] 3 [7 4] 2 [7 2][7 6]))))
    (is (= 1 (count (intersect-circle-arc [5 5] 3 [7 4] 2 [5 4][9 4]))))
))

(deftest intersect-arcs-test
  (testing "intersect-circle-arc-test"
    (is (= 2 (count (intersect-arcs [5 5] 3 [5 2][5 8] [7 4] 2 [5 4][7 6]))))
    (is (= 1 (count (intersect-arcs [5 5] 3 [5 2][5 8] [7 4] 2 [7 2][7 6]))))
    (is (= 1 (count (intersect-arcs [5 5] 3 [2 5][8 5] [7 4] 2 [5 4][7 6]))))
    (is (= 0 (count (intersect-arcs [5 5] 3 [5 8][5 2] [7 4] 2 [5 4][7 6]))))
))

(deftest intersect-straight-lines-test
  (testing "intersect-straight-lines"
    (is (empty? (intersect-straight-lines [100 100][100 600][100 100][100 600])))
    (is (empty? (intersect-straight-lines [100 100][100 600][300 700][300 -42])))
    (is (empty? (intersect-straight-lines [100 100][600 100][3.7 100][15 100])))
    (is (empty? (intersect-straight-lines [100 100][700 700][30 30][-14.0 -14.0])))
    (let [[p1] (intersect-straight-lines [-10.0 0.0][10.0 0.0][0.0 -40.0][0.0 -42.0])]
      (is (equals? 0.0 (length (vec-sub p1 [0.0 0.0])))))
    (let [[p2] (intersect-straight-lines [-1.0 -1.0][1.0 1.0][-1.0 1.0][1.0 -1.0])]
      (is (equals? 0.0 (length (vec-sub p2 [0.0 0.0])))))
    (let [[p3] (intersect-straight-lines [-10.0 -10.0][10.0 10.0][-8.0 8.0][42.0 -42.0])]
      (is (equals? 0.0 (length (vec-sub p3 [0.0 0.0])))))
    (let [[p4] (intersect-straight-lines [-2.0 -2.0][2.0 2.0][-1.0 1.0][2.0 -2.0])]
      (is (equals? 0.0 (length (vec-sub p4 [0.0 0.0])))))
    ))

(deftest intersect-lines-test
  (testing "intersect-lines"
    (is (empty? (intersect-lines [100 100][100 600][100 100][100 600])))
    (is (empty? (intersect-lines [100 100][100 600][300 700][300 -42])))
    (is (empty? (intersect-lines [100 100][600 100][3.7 100][15 100])))
    (is (empty? (intersect-lines [100 100][700 700][30 30][-14.0 -14.0])))
    (is (empty? (intersect-lines [-10.0 0.0][10.0 0.0][0.0 -40.0][0.0 -42.0])))
    (let [[p1] (intersect-lines [-10.0 0.0][10.0 0.0][0.0 42.0][0.0 -42.0])]
      (is (equals? 0.0 (length (vec-sub p1 [0.0 0.0])))))
    (let [[p2] (intersect-lines [-10.0 -10.0][10.0 10.0][-8.0 8.0][42.0 -42.0])]
      (is (equals? 0.0 (length (vec-sub p2 [0.0 0.0])))))
    ))

(deftest intersect-straight-line-circle-test
  (testing "intersect-straight-line-circle: two common points"
    (is (not-any? false? (map equals? [[0.707107 -0.707107][-0.707107 0.707107]] (intersect-straight-line-circle [-0.5 0.5][0.5 -0.5][0.0 0.0] 1.0))))
    (is (not-any? false? (map equals? [[0.707107 0.292893][-0.707107 1.707107]] (intersect-straight-line-circle [-0.5 1.5][0.5 0.5][0.0 1.0] 1.0))))
    (is (not-any? false? (map equals? [[1.707107 -0.707107][0.292893 0.707107]] (intersect-straight-line-circle [0.5 0.5][1.5 -0.5][1.0 0.0] 1.0))))
    (is (not-any? false? (map equals? [[6.414214 6.414214][3.585786 3.585786]] (intersect-straight-line-circle [0.0 0.0][10.0 10.0][5.0 5.0] 2.0))))
    (is (not-any? false? (map equals? [[-0.762380 2.152476][-3.468389 2.693678]] (intersect-straight-line-circle [-5 3][0 2][-2 3] 1.5))))
    (is (= [[300.0 400.0][300.0 200.0]] (intersect-straight-line-circle [300.0 -1000.0][300.0 1000.0][300.0 300.0] 100.0)))
    (is (= [[400.0 300.0][200.0 300.0]] (intersect-straight-line-circle [-1000.0 300.0][1000.0 300.0][300.0 300.0] 100.0))))
  (testing "intersect-straight-line-circle: one common point"
    (is (= [[0.0 5.0]] (intersect-straight-line-circle [-100.0 5.0][100.0 5.0][0.0 0.0] 5.0))))
  (testing "intersect-straight-line-circle: no common point"
    (is (empty? (intersect-straight-line-circle [0.0 0.0][1000.0 1000.0][300.0 500.0] 100.0)))
    (is (empty? (intersect-straight-line-circle [100.0 -1000.0][100.0 1000.0][300.0 300.0] 100.0)))
    (is (empty? (intersect-straight-line-circle [-1000.0 100.0][1000.0 100.0][300.0 300.0] 100.0)))))

(deftest intersect-line-circle-test
  (testing "two common points on line and circle"
    (is (= 2 (count (intersect-line-circle [-5 3][0 2][-2 3] 1.5))))
    )
  (testing "two common points on straight line but only one between p1 and p2"
    (is (= 1 (count (intersect-line-circle [-5 3][-2.5 2.5][-2 3] 1.5))))
    (is (= 1 (count (intersect-line-circle [-2.5 2.5][0 2] [-2 3] 1.5))))
    )
  (testing "two common points on straight line but none between p1 and p2"
    (is (= 0 (count (intersect-line-circle [0 2][5 1][-2 3] 1.5))))
    )
  (testing "one common point on line between p1 and p2"
    (is (= [[-2.0 1.5]] (intersect-line-circle [-5 1.5][5 1.5][-2 3] 1.5)))
    )
  (testing "one common point on straight line but not between p1 and p2"
    (is (empty? (intersect-line-circle [0 1.5][5 1.5][-2 3] 1.5)))
    )
  )

(deftest intersect-line-arc-test
  (testing "two common points on line and arc"
    (is (= 2 (count (intersect-line-arc [0 0][4 2][3 2] 1 [2 2][3 3]))))
    )
  (testing "two common points on full circle but only one on arc"
    (is (= 1 (count (intersect-line-arc [0 0][4 2][3 2] 1 [3 1][3 3]))))
    )
  (testing "two common points on full circle but none on arc"
    (is (= 0 (count (intersect-line-arc [0 0][4 2][3 2] 1 [3 3][2 2]))))
    )
  )

(deftest circumcircle-test
  (testing "circumcircle"
    (let [[p r] (circumcircle [5 6][17 2][12 21])]
      (is (and
            (equals? p [13.40384 11.21153])
            (equals? r 9.88861))))))

(deftest round-test
  (testing "round"
    (is (= 10 (round 10)))
    (is (= 10 (round 10.0)))
    (is (= 10 (round 10.4)))
    (is (= 11 (round 10.5)))))

(deftest box-test
  (testing "box"
    (let [coll [[-1 5][3 -3][0 0][-2 -3][5 -5][-5 2]]
          res (box coll)]
      (is (equals? [-5 -5] (first res)))
      (is (equals? [5 5] (last res))))
    (let [coll [[13 44 -7 66][-3 -42 99 -14][41 -12 -45 19][0 17 0 -55]]
          res (box coll)]
      (is (equals? [-3 -42 -45 -55] (first res)))
      (is (equals? [41 44 99 66] (last res))))
    (let [coll [[-13 -44 -7 -66][-3 -42 -99 -14][-41 -12 -45 -19][0 -17 0 -55]]
          res (box coll)]
      (is (equals? [-41 -44 -99 -66] (first res)))
      (is (equals? [0 -12 0 -14] (last res))))))


