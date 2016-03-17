(ns georepl.mathlib-test
  (:require [clojure.test :refer :all]
            [georepl.mathlib :refer :all]))

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
  (testing "equals"
    (is (true? (equals? 0 0.0)))
    (is (true? (equals? 0.0 0)))
    (is (true? (equals? 5 5.0)))
    (is (false? (equals? 2.0 5.0)))
    (is (true? (equals? 12.369317054748535 12.36931687685298)))
    (is (true? (equals? 42.0 42.0)))))

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
      (is (vec-not-equals? p q))
      (is (vec-not-equals? v w))
      (is (not (vec-not-equals? p p)))
      (is (not (vec-not-equals? w w))))

    (testing "vec-scale"
      (let [p-ref1 p
            p-ref2 q
            p-ref3 (vec-scal-mult
                     0.5
                     (vec-add p q))]
        (is (vec-equals? p (vec-scale p-ref1 p 2.0)))
        (is (vec-equals? q (vec-scale p-ref2 q 2.0)))
        (is (vec-equals? [-4.65 7.45] (vec-scale p-ref3 p 2.0)))
        (is (vec-equals? [5.55 -1.15] (vec-scale p-ref3 q 2.0)))))


    (testing "orthogonal vectors"
      (is (vec-equals? [-1.5 1.5] (vec-ortho v)))
      (is (vec-equals? [-1.0 0.0] (vec-ortho w))))

    (testing "length and dist"
      (is (= 10.0 (length [6 8])))
      (is (= 325.0 (length [0 325])))
      (is (= 18.0 (length [18 0])))
      (is (= 10.0 (length [-6 8])))
      (is (= 18.0 (length [-18 0])))
      (is (= 10.0 (length [6 -8])))
      (is (= 325.0 (length [0 -325])))
      (is (= 10.0 (length [-6 -8])))
      (is (= 0.0 (length [0 0])))
      (is (equals? 2.1213203435596424 (length v)))
      (is (equals? 1 (length w)))
      (is (equals? 6.670832032063167 (dist p q))))

    (testing "dot-product"
      (is (equals? 0.0 (dot-product [1 0][0 1])))
      (is (equals? 1.0 (dot-product [1 0][1 0]))))

    (testing "angle"
      (is (nearly-zero? (angle [0 1][0 1])))
      (is (equals? PI-HALF (angle [1 0][0 1])))
      (is (equals? (* -1 PI-HALF) (angle [0 1][1 0])))
      (is (nil? (angle [0 0][1 1])))
      (is (equals? (/ PI 4) (angle [42 42])))
      (is (nil? (angle [0 1][0 0])))
      (is (nearly-zero? (angle [574 0]))))

    (testing "angle-dir-test"
      (is (equals? 0.3947911196997611 (angle-dir [3 2][2 3])))
      )))


(deftest vec-rotate-center-test
  (let [p [0.0 5]
        q [-1.0 1]]
  (testing "vec-rotate-center"
    (is (vec-equals? [-5.0 0.0] (vec-rotate-center p PI-HALF)))
    (is (vec-equals? [0.0 -5.0] (vec-rotate-center p PI)))
    (is (vec-equals? [-1.0 -1.0] (vec-rotate-center q PI-HALF)))
    (is (vec-equals? [1.0 -1.0] (vec-rotate-center q PI))))))

(deftest vec-rotate-test
  (let [p [0.0 5]
        q [-1.0 1]
        ct [2 -2]]
  (testing "vec-rotate"
    (is (vec-equals? [-5.0 -4.0] (vec-rotate p ct PI-HALF)))
    (is (vec-equals? [4.0 -9.0] (vec-rotate p ct PI)))
    (is (vec-equals? [-1.0 -5.0] (vec-rotate q ct PI-HALF)))
    (is (vec-equals? [5.0 -5.0] (vec-rotate q ct PI))))))

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

(deftest intersect-circles-test
  (testing "intersect-circles-test"
    (let [[p q] (intersect-circles [50 50] 100 [80 60] 80)]
    (is (and
          (vec-equals? p [140.70253 138.10760])
          (vec-equals? q [97.29746 138.10760]))))))

(deftest intersect-lines-test
  (testing "intersect-lines"
    (is (nil? (intersect-lines [[100 100] [100 600]][[300 700] [300 -42]])))
    (is (nil? (intersect-lines [[100 100] [600 100]][[3.7 100] [15 100]])))
    (is (nil? (intersect-lines [[100 100] [700 700]][[30 30] [-14.0 -14.0]])))
    (is (equals? 0.0 (length
                        (vec-sub (intersect-lines [[-10.0 0.0] [10.0 0.0]][[0.0 -40.0] [0.0 -42.0]]) [0.0 0.0]))))
    (is (equals? 0.0 (length
                        (vec-sub (intersect-lines [[-1.0 -1.0] [1.0 1.0]][[-1.0 1.0] [1.0 -1.0]]) [0.0 0.0]))))
    (is (equals? 0.0 (length
                        (vec-sub (intersect-lines [[-10.0 -10.0] [10.0 10.0]][[-8.0 8.0] [42.0 -42.0]]) [0.0 0.0]))))
    (is (equals? 0.0 (length
                        (vec-sub (intersect-lines [[-2.0 -2.0] [2.0 2.0]][[-1.0 1.0] [2.0 -2.0]]) [0.0 0.0]))))
    ))

(deftest intersect-line-segments-test
  (testing "intersect-line-segments"
    (is (nil? (intersect-line-segments [[100 100] [100 600]][[300 700] [300 -42]])))
    (is (nil? (intersect-line-segments [[100 100] [600 100]][[3.7 100] [15 100]])))
    (is (nil? (intersect-line-segments [[100 100] [700 700]][[30 30] [-14.0 -14.0]])))
    (is (nil? (intersect-line-segments [[-10.0 0.0] [10.0 0.0]][[0.0 -40.0] [0.0 -42.0]])))
    (is (equals? 0.0 (length
                        (vec-sub (intersect-line-segments [[-10.0 0.0] [10.0 0.0]][[0.0 40.0] [0.0 -42.0]]) [0.0 0.0]))))
    (is (equals? 0.0 (length
                        (vec-sub (intersect-line-segments [[-10.0 -10.0] [10.0 10.0]][[-8.0 8.0] [42.0 -42.0]]) [0.0 0.0]))))
    ))

(deftest circumcircle-test
  (testing "circumcircle"
    (let [[p r] (circumcircle [5 6][17 2][12 21])]
      (is (and
            (vec-equals? p [13.40384 11.21153])
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
      (is (vec-equals? [-5 -5] (first res)))
      (is (vec-equals? [5 5] (last res))))
    (let [coll [[13 44 -7 66][-3 -42 99 -14][41 -12 -45 19][0 17 0 -55]]
          res (box coll)]
      (is (vec-equals? [-3 -42 -45 -55] (first res)))
      (is (vec-equals? [41 44 99 66] (last res))))
    (let [coll [[-13 -44 -7 -66][-3 -42 -99 -14][-41 -12 -45 -19][0 -17 0 -55]]
          res (box coll)]
      (is (vec-equals? [-41 -44 -99 -66] (first res)))
      (is (vec-equals? [0 -12 0 -14] (last res))))))

(deftest smoothness-test
  (testing "smoothness"
    (let [coll [[-5 2][-5 1][-4 0][-4 -1][-3 -1][-3 -2][-1 -2][-1 -3][0 -3][1 -2][2 -1][3 -1][3 0][4 0][4 2][5 2]]]
      (is (equals? 0.85714 (smoothness coll))))))
