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
(def drw (#'shapes/constructCompound [] :subtype :drawing :filename "/testfiles/temp.grl"))

;; intersections:
;;
;; e5: no intersection
;; e1 x e3 : [[567 524]]
;; e2 x e4 : [[542.648790 506.478054]]
;; e2 x e3 : [[546.063027 502.514117]]
;; e3 x e4 : [[556.690141 513.419845]]
;;


(deftest push-drawing-test
  (testing "push-drawing"
    (is (= (#'elements/push-drawing drw nil) (:drw-elem (#'elements/tos))))
    (is (= (#'elements/push-elem e1) (#'elements/newest-shape)))
    ))


(deftest push-and-pop
  (#'elements/clear)
  (#'elements/push-elem drw)
  (#'elements/push-elem e1)
  (#'elements/push-elem e2)
  (#'elements/push-elem (assoc e3 :name "E42"))
  (is (= 4 (#'elements/elements-length)))
  (is (= e3 (dissoc (#'elements/newest-shape) :name)))
  (#'elements/pop-elem)
  (is (nil? (#'elements/find-element-by-name "E42"))))


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


(deftest unique-name-test
  (elements/push-elem drw)
  (elements/push-elem (assoc e1 :name (elements/unique-name "Pnt")))
  (elements/push-elem (assoc e2 :name (elements/unique-name "Cir")))
  (elements/push-elem (assoc e3 :name (elements/unique-name "Ln")))
  (elements/push-elem (assoc e4 :name (elements/unique-name "Ln")))
  (elements/push-elem (assoc e5 :name (elements/unique-name "Ln")))
  (elements/push-elem (assoc e5 :name (elements/unique-name "Con")))
  (elements/push-elem (assoc e5 :name (elements/unique-name "Con")))
  (is (= "Ln4" (elements/unique-name "Ln")))
  (is (= "Con3" (elements/unique-name "Con")))
  (is (= "Pnt2" (elements/unique-name "Pnt")))
  (is (= "Cir2" (elements/unique-name "Cir")))
  (is (= "Arc1" (elements/unique-name "Arc")))
  (elements/push-elem (assoc e1 :name "Pnt99"))
  (elements/push-elem (assoc e1 :name "Arc42"))
  (elements/push-elem (assoc e1 :name "Cir22"))
  (elements/push-elem (assoc e1 :name "Con12"))
  (elements/push-elem (assoc e1 :name "Ln11"))
  (is (= "Ln12" (elements/unique-name "Ln")))
  (is (= "Con13" (elements/unique-name "Con")))
  (is (= "Pnt100" (elements/unique-name "Pnt")))
  (is (= "Cir23" (elements/unique-name "Cir")))
  (is (= "Arc43" (elements/unique-name "Arc"))))



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

(def lne1 (#'shapes/constructLine [2 2][12 2]))
(def lne2 (#'shapes/constructLine [12 2][5 6]))
(def lne3 (#'shapes/constructLine [5 6][2 2]))
(def lne4 (#'shapes/constructLine [6 0][8 10]))
(def lne5 (#'shapes/constructLine [2 6][0 12]))
(def cle1 (#'shapes/constructCircle [7 5] 4))
(def arc1 (#'shapes/constructArc [11 0] 4 [11 4][0 7]))
(def elems [lne4 lne2 lne5 cle1 lne1 arc1 lne3])



(comment
(deftest create-points-list-test
  (testing "create-points-list"
    (is (= 22 (count (#'elements/create-points-list elems))))
    (#'elements/clear)[[567 524]]
[[542.648790 506.478054]]
[[546.063027 502.514117]]
[[556.690141 513.419845]]
    (#'elements/push-elem drw)
    (is (= 0 (count (#'elements/create-points-list (#'elements/list-elems)))))
    (#'elements/push-elem e1)
    (is (= 0 (count (#'elements/create-points-list (#'elements/list-elems)))))
;;    (#'elements/push-elem e2)
;;    (is (= 1 (count (#'elements/create-points-list (#'elements/list-elems)))))
))
;;    (#'elements/push-elem e3)
;;    (#'elements/push-elem (assoc e4 :name "E4"))
;;    (#'elements/push-elem (assoc e5 :name "E5"))
;;    (is (= 1 (count (#'elements/create-points-list elems))))
;;    (is (= 5 (count (#'elements/create-points-list (#'elements/list-elems)))))))
)
