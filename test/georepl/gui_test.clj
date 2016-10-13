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


(deftest attributes-test
  (let [attr (#'gui/attributes)]
    (is (= {:show-pointlist? false :show-context? false :text-visible? true} (#'gui/attributes)))
    (is (= {:show-pointlist? false :show-context? false :text-visible? true}
           (#'gui/attributes {:show-pointlist? false :show-context? false :text-visible? true})))
    (is (= {:show-pointlist? false :show-context? true :text-visible? true}
           (#'gui/attributes {:show-pointlist? false :show-context? true :text-visible? true})))
    ))

(deftest dash-speed-test
  (is (= true (#'gui/dash-speed? [[0.0 1.0 1000][1.0 0.5 994][2.0 0.25 983][3.0 0.125 972][4.0 0.06125 996]])))
  (is (= true (#'gui/dash-speed? [[0.0 1.0 10000][0.1 0.5 20000][0.2 0.25 30000][0.3 0.125 40000][0.4 0.06125 50000]])))
  (is (= false (#'gui/dash-speed? '([588 140 1458224856243][589 142 1458224856170][588 148 1458224856160][584 152 1458224855574][584 152 1458224855341]))))
  )

(deftest next-point-on-element-test
  (is (= [1.0 0.0] (#'gui/next-point-on-element [[1.0 0.0][0.0 1.0][-1.0 0.0][0.0 -1.0]] [0.0 0.0])))
  (is (= [0.0 -0.7] (#'gui/next-point-on-element [[1.0 0.0][0.0 0.9][-0.8 0.0][0.0 -0.7]] [0.0 0.0]))))

(deftest next-element
  (let [coll [#georepl.shapes.Line{:p1 [325 231], :p2 [162 409], :type :line, :visible 1, :p-ref [154 409], :ortho-polyline? false, :name "Ln1"}
              #georepl.shapes.Line{:p1 [162 409], :p2 [453 492], :type :line, :visible 1, :p-ref [457 492], :ortho-polyline? false, :name "Ln2"}
              #georepl.shapes.Line{:p1 [453 492], :p2 [325 231], :type :line, :visible 1, :p-ref [369 261], :ortho-polyline? false, :name "Ln3"}
              #georepl.shapes.Circle{:p-center [323.7589893918288 393.49559140937134], :radius 162.50033026062113, :type :circle, :visible 1, :p-ref [323.7589893918288 393.49559140937134], :name "Cir1"}]]
    (is (= "Cir1" (:name (first (#'gui/next-element  coll [300 300])))))
    (is (= "Ln1" (:name (first (#'gui/next-element  (butlast coll) [200 300])))))
    ))

;; get-shape-test

;((wrap reset-state) (->Drawing [] (drawing-dialog) (attributes)))

;  (#'fw/init-renderer :test)
;  (testing "setup-gui"
;    (let [state (#'fw/setup-gui)]
;      (is (= georepl.gui.Drawing (type state)))
;      (testing "draw-gui-test"
;        (is (= georepl.gui.Drawing (#'fw/draw-gui state)))))))

