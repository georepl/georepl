(ns georepl.mathlib-test
  (:require [clojure.test :refer :all]
            [georepl.mathlib :refer :all]))

(deftest is-zero-test
  (testing "is-zero"
    (is (true? (is-zero? 0)))
    (is (true? (is-zero? -0)))
    (is (true? (is-zero? 0.0)))
    (is (true? (is-zero? -0.0)))
    (is (true? (is-zero? (first (drop 1000 (iterate (partial * 0.5) 1))))))))

(deftest is-equal-test
  (testing "is-equal"
    (is (true? (equals? 0 0.0)))
    (is (true? (equals? 0.0 0)))
    (is (true? (equals? 5 5.0)))
    (is (false? (equals? 2.0 5.0)))
    (is (true? (equals? 12.369317054748535 12.36931687685298)))
    (is (true? (equals? 42.0 42.0)))))


(deftest difference-test
  (testing "difference"
    (is (= [-117 -391] (difference [134 420] [17 29])))
    (is (= [0 124] (difference [367 5] [367 129])))
    (is (= [-222 -12] (difference [0 -17] [-222 -29])))
    (is (= [-44 -42] (difference [44 42] [0 0])))
    (is (= [17 29] (difference [0 0] [17 29])))
    (is (= [0 0] (difference [0 0] [0 0])))))

(deftest length-test
  (testing "length"
    (is (= 10.0 (length [6 8])))
    (is (= 325.0 (length [0 325])))
    (is (= 18.0 (length [18 0])))
    (is (= 10.0 (length [-6 8])))
    (is (= 18.0 (length [-18 0])))
    (is (= 10.0 (length [6 -8])))
    (is (= 325.0 (length [0 -325])))
    (is (= 10.0 (length [-6 -8])))
    (is (= 0.0 (length [0 0])))))


(deftest angle-test
  (testing "angle"
    (is (is-zero? (angle [0 1][0 1])))
    (is (equals? (/ PI 2) (angle [0 1][1 0])))
    (is (nil? (angle [0 0][1 1])))
    (is (equals? (/ PI 4) (angle [42 42]))))
    (is (nil? (angle [0 1][0 0])))
    (is (is-zero? (angle [574 0])))
  (testing "angle"
    (is (is-zero? (angle [0 1][0 1])))
    (is (equals? (* -1 (/ PI 2)) (angle [0 1][1 0])))
    (is (nil? (angle [0 0][1 1])))
    (is (equals? (/ PI 4) (angle [42 42])))
    (is (is-zero? (angle [574 0])))
    (is (nil? (angle [0 1][0 0])))
    (is (is-zero? (+ (angle [-1 1][-1 0]) (angle [-1 1][0 1]))))
    ))

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
                        (difference
                         (project-point-onto-circle [400 400] [200 200] 100)
                         (project-point-onto-circle [203.2 203.2] [200 200] 100)))))
    ))


(deftest intersect-lines-test
  (testing "intersect-lines"
    (is (nil? (intersect-lines [[100 100] [100 600]][[300 700] [300 -42]])))
    (is (nil? (intersect-lines [[100 100] [600 100]][[3.7 100] [15 100]])))
    (is (nil? (intersect-lines [[100 100] [700 700]][[30 30] [-14.0 -14.0]])))
    (is (equals? 0.0 (length
                        (difference [0.0 0.0] (intersect-lines [[-10.0 0.0] [10.0 0.0]][[0.0 -40.0] [0.0 -42.0]])))))
    (is (equals? 0.0 (length
                        (difference [0.0 0.0] (intersect-lines [[-1.0 -1.0] [1.0 1.0]][[-1.0 1.0] [1.0 -1.0]])))))
    (is (equals? 0.0 (length
                        (difference [0.0 0.0] (intersect-lines [[-10.0 -10.0] [10.0 10.0]][[-8.0 8.0] [42.0 -42.0]])))))
    (is (equals? 0.0 (length
                        (difference [0.0 0.0] (intersect-lines [[-2.0 -2.0] [2.0 2.0]][[-1.0 1.0] [2.0 -2.0]])))))
    ))

(deftest intersect-line-segments-test
  (testing "intersect-line-segments"
    (is (nil? (intersect-line-segments [[100 100] [100 600]][[300 700] [300 -42]])))
    (is (nil? (intersect-line-segments [[100 100] [600 100]][[3.7 100] [15 100]])))
    (is (nil? (intersect-line-segments [[100 100] [700 700]][[30 30] [-14.0 -14.0]])))
    (is (nil? (intersect-line-segments [[-10.0 0.0] [10.0 0.0]][[0.0 -40.0] [0.0 -42.0]])))
    (is (equals? 0.0 (length
                        (difference [0.0 0.0] (intersect-line-segments [[-10.0 0.0] [10.0 0.0]][[0.0 40.0] [0.0 -42.0]])))))
    (is (equals? 0.0 (length
                        (difference [0.0 0.0] (intersect-line-segments [[-10.0 -10.0] [10.0 10.0]][[-8.0 8.0] [42.0 -42.0]])))))
    ))

(deftest right-from?-test
  (testing "right-from?"
    (is (right-from? [[0.0 0.0][42.0 42.0]] [22.0 1.0]))
    ))
