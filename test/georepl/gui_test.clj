(ns georepl.gui-test
  (:require [clojure.test :refer :all]
            [georepl.mathlib :as math]
            [georepl.shapes :as shapes]
            [georepl.gui :as gui]))



(deftest helpers-test
  (testing "snap-time-exceeded?"
    (is (false? (#'gui/snap-time-exceeded? (System/currentTimeMillis))))
    (is (false? (#'gui/snap-time-exceeded? (- (System/currentTimeMillis) 498))))
    (is (false? (#'gui/snap-time-exceeded? (- (System/currentTimeMillis) 700))))
    (is (#'gui/snap-time-exceeded? (- (System/currentTimeMillis) 1002)))
    (is (#'gui/snap-time-exceeded? (- (System/currentTimeMillis) 100000)))
    )

  (testing "short-trace"
    (is (#'gui/short-trace? 0))
    (is (#'gui/short-trace? 4))
    (is (#'gui/short-trace? 15))
    (is (not (#'gui/short-trace? 21)))
    (is (not (#'gui/short-trace? 100)))
    ))



;;(def l0 (assoc (shapes/constructLine [12.0 2.0][50.0 25.0]) :name "Ln0"))
;;(def c0 (assoc (shapes/constructCircle [0.0 0.0] 10.0) :name "Cir0"))
;;(def a0 (assoc (shapes/constructArc [25.0 20.0] 5.0 [30.0 20.0][20.0 20.0]) :name "Arc0"))
;;(def l1 (assoc (shapes/constructLine [-4.0 -4.0][4.0 4.0]) :name "Ln1"))
;;(def c1 (assoc (shapes/constructCircle [0.0 0.0] 10.0) :name "Cir1"))  ;; same as c0
;;(def a1 (assoc (shapes/constructArc [10.0 0.0] 10.0 [10.0 10.0][0.0 0.0]) :name "Arc1"))

(def l0 (shapes/constructLine [12.0 2.0][50.0 25.0]))
(def c0 (shapes/constructCircle [0.0 0.0] 10.0))
(def a0 (shapes/constructArc [25.0 20.0] 5.0 [30.0 20.0][20.0 20.0]))
(def l1 (shapes/constructLine [-4.0 -4.0][4.0 4.0]))
(def c1 (shapes/constructCircle [0.0 0.0] 10.0))  ;; same as c0
(def a1 (shapes/constructArc [10.0 0.0] 10.0 [10.0 10.0][0.0 0.0]))

(def p-list [[12.0 2.0][50.0 25.0][30.0 20.0][20.0 20.0][-4.0 -4.0][4.0 4.0][10.0 10.0][0.0 0.0]])

(deftest cut-elements-test
  (testing "dash line doesn't touch a shape"
    (is (empty? (#'gui/cut-elements [-20 0] [-10 20] [l0 c0 a0] p-list))))
  (testing "dash line intersects a line with no intersection points"
    (is (=
          [:delete l0]
          (#'gui/cut-elements [20 0] [0 20] [l0 c0 a0] p-list))))
    (is (=
          [:delete l0]
          (#'gui/cut-elements [20 0] [0 20] [l0 c0 a0] [])))
  (testing "dash line intersects an arc with no intersection points"
    (is (=
          [:delete a0]
          (#'gui/cut-elements [29 20] [35 30] [l0 c0 a0] p-list))))
  (testing "dash line intersects a circle with no intersection points"
    (is (=
          [:delete c0]
          (#'gui/cut-elements [5 -20] [-5 20] [l0 c0 a0] p-list))))
  (testing "dash line intersects a line with one intersection point"
    (is (=
          [:delete l0
           :create (assoc (shapes/constructLine [31 13.5] [50.0 25.0]) :name nil)]
          (#'gui/cut-elements [30 0] [0 30] [l0 c0 a0] (concat p-list [[31 13.5]])))))
  (testing "dash line intersects an arc with one intersection point"
    (is (=
          [:delete a0
           :create (assoc (shapes/constructArc [25.0 20.0] 5.0 [25 25][20.0 20.0]) :name nil)]
          (#'gui/cut-elements [23 22] [40 40] [l0 c0 a0] (concat p-list [[25 25][31 13.5]])))))
  (testing "dash line intersects a circle with one intersection point"
    (is (=
          [:delete c0]
          (#'gui/cut-elements [-10 -10] [-5 -5] [l0 c0 a0] (concat p-list [[25 25][31 13.5][0 -10]])))))
  (testing "dash line intersects a line with intersection points"
    (is (=
          [:delete c0]
          (#'gui/cut-elements [-10 -10] [-5 -5] [l0 c0 a0] (concat p-list [[25 25][31 13.5][0 10]]))))
    (is (=
          [:delete c0
           :create (shapes/constructArc [0.0 0.0] 10.0 [0 -10] [0 10])]
          (#'gui/cut-elements [-10 -10] [-5 -5] [l0 c0 a0] (concat p-list [[25 25][31 13.5][0 -10][0 10]]))))
    (is (=
          [:delete c0
           :create (shapes/constructArc [0.0 0.0] 10.0 [0 -10] [0 10])]
          (#'gui/cut-elements [-10 -10] [-5 -5] [l0 c0 a0] (concat p-list [[25 25][31 13.5][0 10][0 -10]])))))
  (testing "dash line intersects an arc with intersection points"
    (is (=
          [:delete a0
           :create (assoc (shapes/constructArc [25.0 20.0] 5.0 [25 25] [20.0 20.0]) :name nil)]
          (#'gui/cut-elements [29 20] [35 30] [l0 c0 a0] (concat p-list [[25 25][31 13.5][0 10][0 -10][30 20][25 25][25 15]]))))
    (is (=
          [:delete a0
           :create (assoc (shapes/constructArc [25.0 20.0] 5.0 [30.0 20.0] [25 25]) :name nil)]
          (#'gui/cut-elements [23 21] [0 50] [l0 c0 a0] (concat p-list [[25 25][31 13.5][0 10][0 -10][30 20][25 25][25 15]])))))
  (testing "dash line intersects a circle with intersection points"
    (is (=
          [:delete c0
           :create (shapes/constructArc [0.0 0.0] 10.0 [0 10] [10 0])]
          (#'gui/cut-elements [0 0] [10 10] [l0 c0 a0] (concat p-list [[0 10][-10 0][0 -10][10 0]]))))
    (is (=
          [:delete c0
           :create (shapes/constructArc [0.0 0.0] 10.0 [0 -10] [-10 0])]
          (#'gui/cut-elements [-10 -10] [10 10] [l0 c0 a0] (concat p-list [[0 10][-10 0][0 -10][10 0]])))))
  (testing "dash line intersects a line and a circle with intersection points"
    (is (=
          [:delete l0
           :delete c0
           :create (shapes/constructArc [0.0 0.0] 10.0 [0 10] [10 0])]
          (#'gui/cut-elements [0 5] [50 5] [l0 c0 a0] (concat p-list [[0 10][-10 0][0 -10][10 0]]))))
    (is (=
          [:delete l0
           :create (assoc (shapes/constructLine [21.5 7.75] [50.0 25.0]) :name nil)
           :delete c0
           :create (shapes/constructArc [0.0 0.0] 10.0 [0 10] [10 0])]
          (#'gui/cut-elements [0 5] [50 5] [l0 c0 a0] (concat p-list [[0 10][-10 0][0 -10][10 0][31 13.5][21.5 7.75][40.5 19.25]]))))
       (is (=
          [:delete c0
           :create (shapes/constructArc [0.0 0.0] 10.0 [0 10] [10 0])
           :delete l0]
          (#'gui/cut-elements [0 5] [50 5] [c0 l0 a0] (concat p-list [[0 10][-10 0][0 -10][10 0]]))))))
