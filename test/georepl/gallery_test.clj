(ns georepl.gallery-test
  (:require [clojure.test :refer :all]
            [georepl.draw-framework :as fw]
            [georepl.shapes :as shapes]
            [georepl.gallery :as gallery]))

(def frames
  (list
    [0 "<test7.grl>" [32.5 32.5] [167.5 167.5] [[32.5 167.5] [32.5 32.5] [167.5 32.5] [167.5 167.5] [32.5 167.5]]]
    [1 "test2.grl" [232.5 32.5] [367.5 167.5] [[232.5 167.5] [232.5 32.5] [367.5 32.5] [367.5 167.5] [232.5 167.5]]]
    [2 "test6.grl" [432.5 32.5] [567.5 167.5] [[432.5 167.5] [432.5 32.5] [567.5 32.5] [567.5 167.5] [432.5 167.5]]]
    [3 "test0.grl" [32.5 232.5] [167.5 367.5] [[32.5 367.5] [32.5 232.5] [167.5 232.5] [167.5 367.5] [32.5 367.5]]]
    [4 "test5.grl" [232.5 232.5] [367.5 367.5] [[232.5 367.5] [232.5 232.5] [367.5 232.5] [367.5 367.5] [232.5 367.5]]]
    [5 "test1.grl" [432.5 232.5] [567.5 367.5] [[432.5 367.5] [432.5 232.5] [567.5 232.5] [567.5 367.5] [432.5 367.5]]]
    [6 "test4.grl" [32.5 432.5] [167.5 567.5] [[32.5 567.5] [32.5 432.5] [167.5 432.5] [167.5 567.5] [32.5 567.5]]]
    [7 "test3.grl" [232.5 432.5] [367.5 567.5] [[232.5 567.5] [232.5 432.5] [367.5 432.5] [367.5 567.5] [232.5 567.5]]]))


(deftest in-box-test
  (is (= (nth frames 0) (#'gallery/in-box 100 150 (nth frames 0)))
  (is (nil? (#'gallery/in-box 300 350 (nth frames 0))))))

(deftest mouse-released-test
  (#'fw/init-renderer :test)
  (let [state {:frames frames :f-on-close #(prn %)}]
    (is (= (assoc state :selected (nth frames 0) :complete true) (#'gallery/mouse-released state {:x 100 :y 150})))
    (is (= (assoc state :selected (nth frames 4) :complete true) (#'gallery/mouse-released state {:x 300 :y 350})))
    (is (= state (#'gallery/mouse-released state {:x 1000 :y 1000})))))

(deftest mouse-moved-test
  (#'fw/init-renderer :test)
  (let [state {:frames frames :f-on-close #(prn %)}]
    (is (= (assoc state :selected (nth frames 0)) (#'gallery/mouse-moved state {:x 100 :y 150})))
    (is (= (assoc state :selected (nth frames 4)) (#'gallery/mouse-moved state {:x 300 :y 350})))
    (is (= state (#'gallery/mouse-moved state {:x 1000 :y 1000})))))

(deftest key-pressed-test
  (#'fw/init-renderer :test)
  (let [state1 {:selected (nth frames 3) :frames frames :f-on-close #(prn %)}
        state2 {:selected (nth frames 0) :frames frames :f-on-close #(prn %)}
        state3 {:selected (nth frames 7) :frames frames :f-on-close #(prn %)}]
    (is (= (assoc state1 :selected (nth frames 2)) (#'gallery/key-pressed state1 :up)))
    (is (= (assoc state2 :selected (nth frames 7)) (#'gallery/key-pressed state2 :up)))
    (is (= (assoc state1 :selected (nth frames 2)) (#'gallery/key-pressed state1 :left)))
    (is (= (assoc state2 :selected (nth frames 7)) (#'gallery/key-pressed state2 :left)))
    (is (= (assoc state1 :selected (nth frames 4)) (#'gallery/key-pressed state1 :down)))
    (is (= (assoc state2 :selected (nth frames 1)) (#'gallery/key-pressed state2 :down)))
    (is (= (assoc state3 :selected (nth frames 0)) (#'gallery/key-pressed state3 :down)))
    (is (= (assoc state1 :selected (nth frames 4)) (#'gallery/key-pressed state1 :right)))
    (is (= (assoc state2 :selected (nth frames 1)) (#'gallery/key-pressed state2 :right)))
    (is (= (assoc state3 :selected (nth frames 0)) (#'gallery/key-pressed state3 :right)))
    (is (= (assoc state1 :complete true) (#'gallery/key-pressed state1 :ok)))
    (is (= state1 (#'gallery/key-pressed state1 :anyunknownkey)))
    ))


(deftest recursive-extract-coll-test
  (let [p1 (shapes/constructPoint [25.0 42.0])
        l1 (shapes/constructLine [12.0 42.0][-17.0 -22.0])
        c1 (shapes/constructCircle [0.0 0.0] 33)
        a1 (shapes/constructArc [0.0 10.0] 25 [0.0 -15.0][25.0 10.0])
        cmp0 (shapes/constructCompound [])
        cmp1 (shapes/constructCompound [p1 l1])
        cmp2 (shapes/constructCompound [cmp1 c1])
        cmp3 (shapes/constructCompound [cmp2 a1])
        cmp4 (shapes/constructCompound [cmp1 cmp2 cmp3])]
    (testing "element is nil"
      (is (= [] (#'gallery/recursive-extract-coll nil))))
    (testing "compound with one shape"
      (is (= [p1] (#'gallery/recursive-extract-coll [p1]))))
    (testing "compound with some shapes"
      (is (= (vec (set [p1 l1])) (#'gallery/recursive-extract-coll [cmp1]))))
    (testing "compound with just one empty compound"
      (is (= [] (#'gallery/recursive-extract-coll [(shapes/constructCompound [cmp0])]))))
    (testing "compound with some shapes and a compound"
      (is (= (vec (set [p1 l1])) (#'gallery/recursive-extract-coll [cmp1]))))
    (testing "compound with some shapes and compounds containing one or more compounds"
      (is (= (vec (set (#'gallery/recursive-extract-coll [a1 cmp2])))
             (vec (set (#'gallery/recursive-extract-coll [cmp3])))))
      (is (= (vec (set (#'gallery/recursive-extract-coll [a1 c1 l1 p1])))
             (vec (set (#'gallery/recursive-extract-coll [cmp3])))))
      (is (= (vec (set (#'gallery/recursive-extract-coll [cmp4])))
             (vec (set (#'gallery/recursive-extract-coll [cmp3]))))))))
