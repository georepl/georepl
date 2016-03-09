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
      (is (= [-117 -391] (vec-sub [134 420] [17 29])))
      (is (= [0 124] (vec-sub [367 5] [367 129])))
      (is (= [-222 -12] (vec-sub [0 -17] [-222 -29])))
      (is (= [-44 -42] (vec-sub [44 42] [0 0])))
      (is (= [17 29] (vec-sub [0 0] [17 29])))
      (is (= [0 0] (vec-sub [0 0] [0 0])))
      (is (= [8.64 -11.52]
             (vec-scal-mult
               2.4
               (vec-sub
                 (vec-add p v)
                 (vec-add q w))))))

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

    (testing "vec-scale"
      (let [p-ref1 p
            p-ref2 q
            p-ref3 (vec-scal-mult
                     0.5
                     (vec-add p q))]
        (is (vec-equals? p (vec-scale p-ref1 p 2.0)))
        (is (vec-equals? q (vec-scale p-ref2 q 2.0)))
        (is (vec-equals? [-4.65 7.45] (vec-scale p-ref3 p 2.0)))
        (is (vec-equals? [5.55 -1.15] (vec-scale p-ref3 q 2.0)))))))


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

(deftest angle-test
  (testing "angle"
    (is (nearly-zero? (angle [0 1][0 1])))
    (is (equals? PI-HALF (angle [1 0][0 1])))
    (is (equals? (* -1 PI-HALF) (angle [0 1][1 0])))
    (is (nil? (angle [0 0][1 1])))
    (is (equals? (/ PI 4) (angle [42 42]))))
    (is (nil? (angle [0 1][0 0])))
    (is (nearly-zero? (angle [574 0])))vec-rotate-center)

(deftest round-test
  (testing "round"
    (is (= 10 (round 10)))
    (is (= 10 (round 10.0)))
    (is (= 10 (round 10.4)))
    (is (= 11 (round 10.5)))))

(deftest det-test
  (testing "det"
    (is (= 0 (det [0 0][0 0])))
    (is (= 0 (det [1 -2][1 -2])))
    (is (= 1 (det [3 -11][-1 4])))
    (is (= 10 (det [5 0][-1 2])))
    (is (= -450 (det [3 -42][-11 4])))
  ))

(deftest degree-test
  (testing "degree"
    (is (equals? 0.0 (degree 0.0)))
    (is (equals? 360.0 (degree TWO-PI)))
    (is (equals? 180.0 (degree PI)))
    (is (equals? 270.0 (degree (* (/ 3 2) PI))))
    ))

(deftest dot-product-test
  (testing "dot-product"
    (is (equals? 0.0 (dot-product [1 0][0 1])))
    (is (equals? 1.0 (dot-product [1 0][1 0])))
    ))

(deftest project-point-onto-circle-test
  (testing "project-point-onto-circle"
    (is (equals? 0.0 (length
                        (vec-sub
                         (project-point-onto-circle [400 400] [200 200] 100)
                         (project-point-onto-circle [203.2 203.2] [200 200] 100)))))
    ))


(deftest intersect-lines-test
  (testing "intersect-lines"
    (is (nil? (intersect-lines [[100 100] [100 600]][[300 700] [300 -42]])))
    (is (nil? (intersect-lines [[100 100] [600 100]][[3.7 100] [15 100]])))
    (is (nil? (intersect-lines [[100 100] [700 700]][[30 30] [-14.0 -14.0]])))
    (is (equals? 0.0 (length
                        (vec-sub [0.0 0.0] (intersect-lines [[-10.0 0.0] [10.0 0.0]][[0.0 -40.0] [0.0 -42.0]])))))
    (is (equals? 0.0 (length
                        (vec-sub [0.0 0.0] (intersect-lines [[-1.0 -1.0] [1.0 1.0]][[-1.0 1.0] [1.0 -1.0]])))))
    (is (equals? 0.0 (length
                        (vec-sub [0.0 0.0] (intersect-lines [[-10.0 -10.0] [10.0 10.0]][[-8.0 8.0] [42.0 -42.0]])))))
    (is (equals? 0.0 (length
                        (vec-sub [0.0 0.0] (intersect-lines [[-2.0 -2.0] [2.0 2.0]][[-1.0 1.0] [2.0 -2.0]])))))
    ))

(deftest intersect-line-segments-test
  (testing "intersect-line-segments"
    (is (nil? (intersect-line-segments [[100 100] [100 600]][[300 700] [300 -42]])))
    (is (nil? (intersect-line-segments [[100 100] [600 100]][[3.7 100] [15 100]])))
    (is (nil? (intersect-line-segments [[100 100] [700 700]][[30 30] [-14.0 -14.0]])))
    (is (nil? (intersect-line-segments [[-10.0 0.0] [10.0 0.0]][[0.0 -40.0] [0.0 -42.0]])))
    (is (equals? 0.0 (length
                        (vec-sub [0.0 0.0] (intersect-line-segments [[-10.0 0.0] [10.0 0.0]][[0.0 40.0] [0.0 -42.0]])))))
    (is (equals? 0.0 (length
                        (vec-sub [0.0 0.0] (intersect-line-segments [[-10.0 -10.0] [10.0 10.0]][[-8.0 8.0] [42.0 -42.0]])))))
    ))

(deftest right-from?-test
  (testing "right-from?"
    (is (right-from? [[0.0 0.0][42.0 42.0]] [22.0 1.0]))
    ))
