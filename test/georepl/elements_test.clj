(ns georepl.elements-test
  (:require [clojure.test :refer :all]
            [georepl.elements :as elements]
            [georepl.shapes :as shapes]
            [georepl.mathlib :as math]
            [georepl.user :as user]))


(def e1 (#'shapes/constructPoint [567 524]))
(def e2 (#'shapes/constructCircle [567 524] 30))
(def e3 (#'shapes/constructLine [300 250][567 524]))
(def e4 (#'shapes/constructLine [125 300][570 520]))
(def e5 (#'shapes/constructLine [224  42][224  24]))
(def e6 (#'shapes/constructArc [100 100] 50 [150 100][100 150]))
(def e7 (#'shapes/constructCompound [e1 e2 e3 e4 e5 e6]))

(def drw (#'shapes/constructCompound [] :subtype :drawing :filename "/testfiles/temp.grl"))

;; intersections:
;;
;; e5: no intersection
;; e1 x e3 : [[567 524]]
;; e2 x e4 : [[542.648790 506.478054]]
;; e2 x e3 : [[546.063027 502.514117]]
;; e3 x e4 : [[556.690141 513.419845]]
;;



(deftest unique-name-test
  (#'elements/push-elem drw)
  (#'elements/push-elem (assoc e1 :name (#'elements/unique-name "Pnt" [])))
  (#'elements/push-elem (assoc e2 :name (#'elements/unique-name "Cir" ["Pnt1"])))
  (#'elements/push-elem (assoc e3 :name (#'elements/unique-name "Ln" ["Pnt1" "Cir1"])))
  (#'elements/push-elem (assoc e4 :name (#'elements/unique-name "Ln" ["Pnt1" "Cir1" "Ln1"])))
  (#'elements/push-elem (assoc e5 :name (#'elements/unique-name "Ln" ["Pnt1" "Cir1" "Ln1" "Ln2"])))
  (#'elements/push-elem (assoc e5 :name (#'elements/unique-name "Con" ["Pnt1" "Cir1" "Ln1" "Ln2" "Ln3"])))
  (#'elements/push-elem (assoc e5 :name (#'elements/unique-name "Con" ["Pnt1" "Cir1" "Ln1" "Ln2" "Ln3" "Con1"])))
  (is (= "Ln4"  (first (#'elements/unique-name "Ln" ["Pnt1" "Cir1" "Ln1" "Ln2" "Ln3" "Con1" "Con2"]))))
  (is (= "Con3" (first (#'elements/unique-name "Con" ["Pnt1" "Cir1" "Ln1" "Ln2" "Ln3" "Con1" "Con2"]))))
  (is (= "Pnt2" (first (#'elements/unique-name "Pnt" ["Pnt1" "Cir1" "Ln1" "Ln2" "Ln3" "Con1" "Con2"]))))
  (is (= "Cir2" (first (#'elements/unique-name "Cir" ["Pnt1" "Cir1" "Ln1" "Ln2" "Ln3" "Con1" "Con2"]))))
  (is (= "Arc1" (first (#'elements/unique-name "Arc" ["Pnt1" "Cir1" "Ln1" "Ln2" "Ln3" "Con1" "Con2"]))))

  (#'elements/push-elem (assoc e1 :name "Pnt99"))
  (#'elements/push-elem (assoc e6 :name "Arc42"))
  (#'elements/push-elem (assoc e2 :name "Cir22"))
  (#'elements/push-elem (assoc e7 :name "Con12"))
  (#'elements/push-elem (assoc e5 :name "Ln11"))


  (is (= "Ln12"   (first (#'elements/unique-name "Ln" ["Pnt99" "Arc42" "Cir22" "Con12" "Ln11"]))))
  (is (= "Con13"  (first (#'elements/unique-name "Con" ["Pnt99" "Arc42" "Cir22" "Con12" "Ln11" "Ln12"]))))
  (is (= "Pnt100" (first (#'elements/unique-name "Pnt" ["Pnt99" "Arc42" "Cir22" "Con12" "Ln11" "ln12" "Con13"]))))
  (is (= "Cir23"  (first (#'elements/unique-name "Cir" ["Pnt99" "Arc42" "Cir22" "Con12" "Ln11" "ln12" "Con13" "Pnt100"]))))
  (is (= "Arc43"  (first (#'elements/unique-name "Arc" ["Pnt99" "Arc42" "Cir22" "Con12" "Ln11" "ln12" "Con13" "Pnt100" "Citr23"])))))

(deftest push-drawing-test
  (testing "push-drawing"
    (is (= (#'elements/push-drawing drw nil) (:drw-elem (#'elements/tos))))
    (is (= (#'elements/push-elem e1) (#'elements/newest-shape)))
    ))


(deftest push-and-pop-test
  (#'elements/clear)
  (#'elements/push-elem drw)
  (#'elements/push-elem e1)
  (#'elements/push-elem e2)
  (#'elements/push-elem (assoc e3 :name "E42"))
  (is (= 4 (#'elements/elements-length)))
  (is (= e3 (dissoc (#'elements/newest-shape) :name)))
  (#'elements/pop-elem)
  (is (nil? (#'elements/find-element-by-name "E42")))
  (#'elements/clear)
  (#'elements/push-elem drw)
  (#'elements/push-elem e2)
  (is (= [e2] (elements/list-elems))))

(deftest push-elems-test
  (#'elements/clear)
  (#'elements/push-elem drw)
  (#'elements/push-elems [e1 e4 e5])
  (is (= 3 (count (#'elements/collect-shapes (:drw-elem (#'elements/tos))))))
  (#'elements/push-elems [e2 e3])
  (is (= 2 (count (#'elements/collect-shapes (:drw-elem (#'elements/tos)))))))

(deftest stack-operations-test
  (testing "elements-length"
    (#'elements/clear)
    (is (= 0 (#'elements/elements-length))))
  (testing "initialize with empty drawing"
    (is (= drw (#'elements/push-elem drw))))
  (testing "elements-length after push-elem"
    (is (= 1 (#'elements/elements-length)))
    (is (= 1 (#'elements/shapes-count)))
    (is (= e1 (#'elements/push-elem e1)))
    (is (= 2 (#'elements/elements-length)))
    (is (= 2 (#'elements/shapes-count)))
    (is (= e2 (#'elements/push-elem e2)))
    (is (= 3 (#'elements/elements-length)))
    (is (= 3 (#'elements/shapes-count)))
    (is (= 2 (count (#'elements/list-elems))))
    (is (= 1 (count (#'elements/list-points)))))
  (testing "tos and newest-shape"
    (let [drw (:drw-elem (#'elements/tos))]
      (is (and (= :compound (:type drw))(= :drawing (:subtype drw))))
      (is (= e2 (#'elements/push-elem e2)))
      (is (= e2 (#'elements/newest-shape)))
      (#'elements/clear)
      (is (nil? (#'elements/tos)))))
  (testing "elements-length after clear"
    (is (= 0 (#'elements/elements-length))))
  (testing "points-list"
    (#'elements/clear)
    (#'elements/push-elem drw)
    (#'elements/push-elem e2)
    (#'elements/push-elem e3)
    (#'elements/push-elem e5)
    (#'elements/push-elem e1)
    (#'elements/push-elem e4)
    (let [pts (#'elements/list-points)]
      (is (not-any? false? (map math/equals? (sort pts)(sort [[125 300][224 24][224 42][300 250][542.648791 506.478054][546.063027 502.514117][556.690141 513.419845][567 524][570 520]])))))))


(deftest collect-XXX-test
  (#'elements/push-elem drw)
  (#'elements/push-elem e1)
  (#'elements/push-elem (assoc e2 :name "E2"))
  (#'elements/push-elem e3)
  (#'elements/push-elem (assoc e4 :name "E4"))
  (#'elements/push-elem (assoc e5 :name "E5"))
  (is (= 5 (count (#'elements/collect-shapes (:drw-elem (#'elements/tos))))))
  (is (= 6 (count (#'elements/collect-elements (:drw-elem (#'elements/tos))))))
  (is (= 3 (count (#'elements/collect-named-elements (:drw-elem (#'elements/tos))))))
  (is (= e4 (dissoc (#'elements/find-element-by-name "E4") :name)))
  (is (nil? (#'elements/find-element-by-name "E8"))))



(deftest persistance-test
  (testing "create an empty drawing"
    (#'elements/clear)
    (is (= drw (#'elements/push-drawing drw nil)))
    (is (= drw (:drw-elem (#'elements/tos))))
    (is (nil? (#'elements/spit-drawing))))
  (testing "create a drawing"
    (is (= e3 (#'elements/push-elem e3)))
    (is (= e2 (#'elements/push-elem e2)))
    (is (= e1 (#'elements/push-elem e1))))
  (testing "spit-drawing"
    (#'elements/spit-drawing)
    (is (= :compound (:type (#'elements/slurp-drawing "workbench/testfiles/temp.grl"))))
    (is (= 5 (#'elements/elements-length)))))


(deftest repl-form-test
  (testing "empty drawing"
    (#'elements/clear)
    (#'elements/push-drawing drw nil)
    (is (nil? (#'elements/reinit-repl-server e1))))
  (testing "drawing with unnamed element"
    (#'elements/register #'user/update-elements)
    (is (nil? (#'elements/reinit-repl-server (dissoc e1 :name)))))
  (testing "repl-form after pushing a shape"
    (#'elements/register #'user/update-elements)
    (is (nil? (#'elements/reinit-repl-server e2))))
    )

;;(def pnt1 (assoc (#'shapes/constructPoint [2 2]) :name "Pnt1"))
;;(def pnt2 (assoc (#'shapes/constructPoint [12 2]) :name "Pnt2"))
;;(def pnt3 (assoc (#'shapes/constructPoint [5 6]) :name "Pnt3"))
;;(def lne1 (assoc (#'shapes/constructLine [2 2][12 2]) :name "Lne1"))
;;(def lne2 (assoc (#'shapes/constructLine [12 2][5 6]) :name "Lne2"))
;;(def lne3 (assoc (#'shapes/constructLine [5 6][2 2]) :name "Lne3"))
;;(def lne4 (assoc (#'shapes/constructLine [6 0][8 10]) :name "Lne4"))
;;(def lne5 (assoc (#'shapes/constructLine [2 6][0 12]) :name "Lne5"))
;;(def cle1 (assoc (#'shapes/constructCircle [7 5] 4) :name "Cle1"))
;;(def arc1 (assoc (#'shapes/constructArc [11 0] 4 [11 4][0 7]) :name "Arc1"))

(def pnt1 (#'shapes/constructPoint [2 2]))
(def pnt2 (#'shapes/constructPoint [12 2]))
(def pnt3 (#'shapes/constructPoint [5 6]))
(def lne1 (#'shapes/constructLine [2 2][12 2]))
(def lne2 (#'shapes/constructLine [12 2][5 6]))
(def lne3 (#'shapes/constructLine [5 6][2 2]))
(def lne4 (#'shapes/constructLine [6 0][8 10]))
(def lne5 (#'shapes/constructLine [2 6][0 12]))
(def cle1 (#'shapes/constructCircle [7 5] 4))
(def arc1 (#'shapes/constructArc [11 0] 4 [11 4][0 7]))


(deftest update-elements-test
  (#'elements/clear)
  (#'elements/push-drawing drw nil)
  (is (empty? (#'elements/collect-shapes (:drw-elem (#'elements/tos)))))
  (#'elements/update-elements [:create lne1 :create lne2 :create lne3])
  (is (= [(assoc lne3 :name "Ln3")(assoc lne2 :name "Ln2")(assoc lne1 :name "Ln1")]
         (#'elements/collect-shapes (:drw-elem (#'elements/tos)))))
  (#'elements/update-elements [:create arc1 :delete (assoc lne2 :name "Ln2")])
  (is (= [(assoc arc1 :name "Arc1")(assoc lne3 :name "Ln3")(assoc lne1 :name "Ln1")]
         (#'elements/collect-shapes (:drw-elem (#'elements/tos)))))
  (#'elements/update-elements [:delete (assoc lne1 :name "Ln1") :create cle1 :create lne2])
  (is (= [(assoc lne2 :name "Ln4")(assoc cle1 :name "Cir1")(assoc arc1 :name "Arc1")(assoc lne3 :name "Ln3")]
         (#'elements/collect-shapes (:drw-elem (#'elements/tos)))))
  (#'elements/update-elements [:delete (assoc lne2 :name "Ln4")])
  (is (= [(assoc cle1 :name "Cir1")(assoc arc1 :name "Arc1")(assoc lne3 :name "Ln3")]
         (#'elements/collect-shapes (:drw-elem (#'elements/tos)))))
  (#'elements/update-elements [:create lne1])
  (is (= [(assoc lne1 :name "Ln4")(assoc cle1 :name "Cir1")(assoc arc1 :name "Arc1")(assoc lne3 :name "Ln3")]
         (#'elements/collect-shapes (:drw-elem (#'elements/tos)))))
  (#'elements/update-elements [:delete (assoc lne1 :name "Ln4")
                               :delete (assoc arc1 :name "Arc1")
                               :delete (assoc cle1 :name "Cir1")
                               :delete (assoc lne2 :name "Ln2")])
  (is (= [(assoc lne3 :name "Ln3")] (#'elements/collect-shapes (:drw-elem (#'elements/tos))))))
