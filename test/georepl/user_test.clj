(ns georepl.user-test
  (:require [clojure.test :refer :all]
            [georepl.shapes :as shapes]
            [georepl.elements :as elements]
            [georepl.user :as user]))

(def drw (#'shapes/constructCompound [] :subtype :drawing :filename "/testfiles/temp.grl"))



(deftest nrepl-base-test
  (testing "start stop"
    (let [[server f] (user/start)]
      (is (or (nil? f)(fn? f)))
      (is (not (nil? server)))
      (is (not (nil? (user/stop server)))))))


(deftest decolour-test
  (let [e1 (shapes/constructLine [100 50][50 -200])
        e2 (shapes/constructCircle [50 0] 50)
        e3 (shapes/constructLine [100 -100][0 0])
        e4 (shapes/constructCompound [e1 e2 e3])
        e5 (shapes/constructCompound (vec (map #(assoc % :colour :green) [e1 e2 e3])))
        e6 (shapes/constructCompound [e5])]
    (is (= e1 (#'user/decolour e1)))
    (is (= e1 (#'user/decolour (assoc e1 :colour :green))))
    (is (= e4 (#'user/decolour e4)))
    (is (= e4 (#'user/decolour e5)))
    (is (= (shapes/constructCompound [e4]) (#'user/decolour e6)))))


(deftest elementary-shapes-test
  (testing "check-arguments"
    (is (true? (#'user/check-arguments [42] [[number? "Oooops!"]])))
    (is (false? (#'user/check-arguments ["42"] [[number? "Oooops!"]])))
    (is (true? (#'user/check-arguments
                  ["42" 42 [1.0 2.0]]
                  [[string? "%s not a string"][number? "%s not a number"][#(and (coll? %) (every? number? %)) "%s not a vector"]])))
    (is (false? (#'user/check-arguments
                  [42.0 42 [1.0 2.0]]
                  [[string? "%s not a string"][number? "%s not a number"][#(and (coll? %) (every? number? %)) "%s not a vector"]])))
    (is (false? (#'user/check-arguments
                  ["42" "fourty-two" [1.0 2.0]]
                  [[string? "%s not a string"][number? "%s not a number"][#(and (coll? %) (every? number? %)) "%s not a vector"]])))
    (is (false? (#'user/check-arguments
                  [42 42 0.0]
                  [[string? "%s not a string"][number? "%s not a number"][#(and (coll? %) (every? number? %)) "%s not a vector"]])))
    (is (false? (#'user/check-arguments
                  ["42" 42 [1.0 2.0] 17]
                  [[string? "%s not a string"][number? "%s not a number"][#(and (coll? %) (every? number? %)) "%s not a vector"]]))))
  (testing "point"
    (#'elements/push-elem drw)
    (is (nil? (#'user/point nil)))
    (is (nil? (#'user/point 17)))
    (is (nil? (#'user/point [17])))
    (is (nil? (#'user/point [17 "42"])))
    (is (nil? (#'user/point [17 42 43])))
    (is (= #georepl.shapes.Point{:p [17 42], :type :point, :visible 1, :p-ref [17 42] :name "Pnt1"}
           (#'user/point [17 42]))))
  (testing "line"
    (is (nil? (#'user/line nil nil)))
    (is (nil? (#'user/line 17 42)))
    (is (nil? (#'user/line [17 42] "bla")))
    (is (nil? (#'user/line [17 42] [0.0])))
    (is (= #georepl.shapes.Line{:p1 [17 42], :p2 [17 42], :type :line, :visible 1, :p-ref [17 42] :name "Ln1"}
           (#'user/line [17 42] [17 42])))
    (is (= #georepl.shapes.Line{:p1 [17 42], :p2 [0.0 15], :type :line, :visible 1, :p-ref [17 42] :name "Ln2"}
           (#'user/line [17 42] [0.0 15]))))
  (testing "circle"
    (is (nil? (#'user/circle nil nil)))
    (is (nil? (#'user/circle 17 42)))
    (is (nil? (#'user/circle [17 42] "bla")))
    (is (nil? (#'user/circle [17 42] [0.0])))
    (is (nil? (#'user/circle 17 [0.0])))
    (is (nil? (#'user/circle [0.0] 42)))
    (is (= #georepl.shapes.Circle{:p-center [0.0 0.0], :radius 42, :type :circle, :visible 1, :p-ref [0.0 0.0], :name "Cir1"}
           (#'user/circle [0.0 0.0] 42)))
    (is (nil? (#'user/circle nil nil nil)))
    (is (nil? (#'user/circle 17 [9.0 7] 42)))
    (is (nil? (#'user/circle "foo" "bla" "bar")))
    (is (nil? (#'user/circle [17 42] 8 [0.0])))
    (is (nil? (#'user/circle 17 [0.0] 42)))
    (is (nil? (#'user/circle [0.0] 42 [42 3])))
    (is (= #georepl.shapes.Circle{:p-center [0.0 -0.0], :radius 4.0, :type :circle, :visible 1, :p-ref [0.0 -0.0], :name "Cir2"}
           (#'user/circle [0.0 -4.0] [4.0 0.0] [0 4])))
    )
  (testing "arc"
    (is (nil? (#'user/arc nil nil nil nil)))
    (is (nil? (#'user/arc 17 [9.0 7] 42 [13 5])))
    (is (= #georepl.shapes.Arc{:p-center [0.0 0.0], :radius 5, :p-start [0.0 5], :p-end [5 0.0], :type :arc, :visible 1, :p-ref [0.0 5] :name "Arc1"}
           (#'user/arc [0.0 0.0] 5 [0.0 5][5 0.0])))
    ))

  (deftest select-test
    (#'elements/push-elem drw)
    (is (nil? (:selected-elem (#'user/select nil))))
    (is (nil? (:selected-elem (#'user/select ""))))
    (is (nil? (:selected-elem (#'user/select "Cir42")))))

  (deftest compound-test
    (#'elements/push-elem drw)
    (let [l1 (#'user/line [100 50][50 -200])
          c1 (#'user/circle [0 50] 60)
          p1 (#'user/point [75 -75])
          cp (#'user/compound "Ln1" "Cir1" "Pnt1")]
      (is (= [(assoc l1 :name "Ln1" :colour :green)(assoc c1 :name "Cir1" :colour :green)(assoc p1 :name "Pnt1" :colour :green)]
             (#'user/show identity :list-shapes)))
      (is (= cp
             (#'user/show identity :selected-elem)))
      (is (nil? (#'user/compound "Ln1" 5)))
      (is (nil? (#'user/compound "Ln1" "Ln7")))
      ))

  (deftest settings-test
    (is (empty? (#'user/settings)))
    (is (not (empty? (#'user/settings :1 :2 :3)))))

  (deftest show-test
    (is (not (nil? (#'user/show identity :selected-elem))))
    (is (not (nil? (#'user/show identity :list-elements))))
    (is (= "mode :unknown-mode not implemented"
           (#'user/show identity :unknown-mode)))
    (is (nil? (#'user/show :unknown-mode))))

  (deftest move-test
    (#'elements/push-elem drw)
    (let [p1 (#'user/point [75 -75])
          l1 (#'user/line [100 50][50 -200])
          c1 (#'user/circle [0 50] 60)
          a1 (#'user/arc [50 0] 50 [50 -50][50 50])
          cmpnd (#'user/compound "Arc1" "Pnt1")]
      (is (= (set [(assoc c1 :name "Cir1")(assoc l1 :name "Ln1")(assoc a1 :name "Arc1" :colour :green)(assoc p1 :name "Pnt1" :colour :green)])
             (set (#'user/show identity :list-shapes))))
        (#'user/select "Cmpnd1")
        (#'user/move [30 40])
        (is (= (set [c1 l1 (assoc (#'shapes/constructArc [80 40] 50 [80 -10][80 90]) :name "Arc1" :colour :green)
                           (assoc (#'shapes/constructPoint [105 -35]) :name "Pnt1" :colour :green)])
               (set (#'user/show identity :list-shapes))))
        (#'user/select "Ln1")
        (#'user/move [-30 40])
        (is (= (set [c1 (assoc l1 :name "Ln1" :p1 [70 90] :p2 [20 -160] :p-ref [70 90] :colour :green)
                        (assoc (#'shapes/constructArc [80 40] 50 [80 -10][80 90]) :name "Arc1")
                        (assoc (#'shapes/constructPoint [105 -35]) :name "Pnt1")])
               (set (#'user/show identity :list-shapes))))
          ))


  (deftest copy-test
    (#'elements/push-elem drw)
    (let [p1 (#'user/point [75 -75])
          l1 (#'user/line [100 50][50 -200])
          c1 (#'user/circle [0 50] 60)
          a1 (#'user/arc [50 0] 50 [50 -50][50 50])
          cmpnd1 (#'user/compound "Ln1" "Cir1")]
      (is (= (set [(assoc c1  :colour :green)(assoc l1  :colour :green) a1 p1])
             (set (#'user/show identity :list-shapes))))
      (#'user/select "Cmpnd1")
      (#'user/copy)
      (let [l2 (assoc l1 :name "Ln2")
            c2 (assoc c1 :name "Cir2")
            cmpnd2 (shapes/constructCompound [l2 c2])]
        (is (= (set [a1 p1
                     (assoc c1 :colour :green)
                     (assoc l1 :colour :green)])
               (set (#'user/show identity :list-shapes))))
        (#'user/select "Arc1")
        (#'user/copy)
        (is (= (set [p1 c1 a1 l1 (assoc a1 :name "Arc2" :colour :green)])
               (set (#'user/show identity :list-shapes))))
        )))

  (deftest rotate-test
    (#'elements/push-elem drw)
    (let [p1 (#'user/point [75.0 -75.0])
          l1 (#'user/line [100.0 50.0][50.0 -200.0])
          c1 (#'user/circle [0.0 50.0] 60)
          a1 (#'user/arc [50.0 0.0] 50 [50.0 -50.0][50.0 50.0])
          cmpnd1 (#'user/compound "Ln1" "Cir1")]
      (is (= (set [(assoc l1 :colour :green) p1 a1 (assoc c1 :colour :green)])
             (set (#'user/show identity :list-shapes))))
      (#'user/select "Cmpnd1")
      (#'user/rotate 2.6)
      (is (= (set [(assoc l1 :p2 [271.71978062381345 238.4471197511636] :colour :green)
                   p1 a1
                   (assoc c1 :p-center [185.68887533689474 -1.5501371821464147] :p-ref [185.68887533689474 -1.5501371821464147] :colour :green)])
             (set (#'user/show identity :list-shapes))))
      (#'user/select "Cir1")
      (#'user/rotate -0.7)
      (is (= (set [(assoc l1 :p2 [271.71978062381345 238.4471197511636])
                   p1 a1
                   (assoc c1 :p-center [185.68887533689474 -1.5501371821464147] :p-ref [185.68887533689474 -1.5501371821464147] :colour :green)])
             (set (#'user/show identity :list-shapes))))
      (#'user/select "Ln1")
      (#'user/rotate -0.7)
      (is (= (set [(assoc l1 :p2 [352.7395002650288 83.50738733150514] :colour :green)
                   p1 a1
                   (assoc c1 :p-center [185.68887533689474 -1.5501371821464147] :p-ref [185.68887533689474 -1.5501371821464147])])
             (set (#'user/show identity :list-shapes))))
        ))

  (deftest scale-test
    (#'elements/push-elem drw)
    (let [p1 (#'user/point [75.0 -75.0])
          l1 (#'user/line [100.0 50.0][50.0 -200.0])
          c1 (#'user/circle [0.0 50.0] 60)
          a1 (#'user/arc [50.0 0.0] 50 [50.0 -50.0][50.0 50.0])
          cmpnd1 (#'user/compound "Ln1" "Cir1" "Pnt1" "Arc1")]
      (#'user/select "Cir1")
      (#'user/scale 2)
      (is (= (set [p1 a1 l1 (assoc c1 :radius 120 :colour :green)])
             (set (#'user/show identity :list-shapes))))
      (#'user/scale 5)
      (is (= (set [p1 a1 l1 (assoc c1 :radius 600 :colour :green)])
             (set (#'user/show identity :list-shapes))))
      (#'user/scale -5)
      (is (= (set [p1 a1 l1 (assoc c1 :radius 600 :colour :green)])
             (set (#'user/show identity :list-shapes))))
      (#'user/select "Ln1")
      (#'user/scale -5)
      (is (= (set [p1 a1 (assoc l1 :colour :green) (assoc c1 :radius 600)])
             (set (#'user/show identity :list-shapes))))
    ))

  (deftest mirror-test
    (#'elements/push-elem drw)
    (let [p1 (#'user/point [75.0 -75.0])
          l1 (#'user/line [100.0 50.0][50.0 -200.0])
          c1 (#'user/circle [0.0 50.0] 60)
          a1 (#'user/arc [50.0 0.0] 50 [50.0 -50.0][50.0 50.0])
          cmpnd1 (#'user/compound "Ln1" "Cir1" "Pnt1" "Arc1")]
      (#'user/select "Cir1")
      (#'user/mirror [0.0 100.0][100.0 0.0])
      (is (= (set [p1 l1 a1
                   (assoc c1 :p-center [50.0 100.0] :p-ref [50.0 100.0] :colour :green)])
             (set (#'user/show identity :list-shapes))))
      (#'user/select "Ln1")
      (#'user/mirror [0.0 100.0][100.0 0.0])
      (is (= (set [p1 a1
                   (assoc l1 :p1 [50.0 0.0] :p2 [300.0 50.0] :p-ref [50.0 0.0] :colour :green)
                   (assoc c1 :p-center [50.0 100.0] :p-ref [50.0 100.0])])
             (set (#'user/show identity :list-shapes))))
    ))


  (deftest undo-test
    (#'elements/clear)
    (#'elements/push-elem drw)
    (#'elements/push-elem (assoc (shapes/constructCompound []) :subtype :drawing :p-ref [0 0] :filename ""))
    (#'user/undo)
      (is (empty? (#'user/show identity :list-shapes)))
    (#'user/undo)
      (is (empty? (#'user/show identity :list-shapes)))
    (let [d (#'elements/push-elem drw)
          p1 (#'user/point [75.0 -75.0])
          l1 (#'user/line [100.0 50.0][50.0 -200.0])
          c1 (#'user/circle [0.0 50.0] 60)
          a1 (#'user/arc [50.0 0.0] 50 [50.0 -50.0][50.0 50.0])
          cmpnd1 (#'user/compound "Ln1" "Cir1" "Pnt1" "Arc1")]
      (is (= 4 (count (#'user/show identity :list-shapes))))
      (#'user/undo)
      (is (= 4 (count (#'user/show identity :list-shapes))))
      (#'user/undo)
      (is (= 3 (count (#'user/show identity :list-shapes))))
      (#'user/undo)
      (is (= 2 (count (#'user/show identity :list-shapes))))
      (#'user/undo)
      (is (= 1 (count (#'user/show identity :list-shapes))))
      (#'user/undo)
      (is (empty? (#'user/show identity :list-shapes)))
    ))
