(ns georepl.shapes-factory
  (:require [clojure.core.match :refer [match]]
            [georepl.shapes :as shapes]
            [georepl.elements :as elements]
            [georepl.mathlib :as math]))


;; functions which are identical for all shapes
;;
(defn- current-strategy [this]
  (first (drop-while #(not (:highlight %))(:strategies this))))

(defn- change-strategy [state name]
  (update-in state
    [:strategies]
    (fn[coll]
      (vec
        (map #(assoc % :highlight (= (:s %) name)) coll)))))

(defn refresh [this p]
  (if-let [question (first (:quector (current-strategy this)))]
    ((:g question) this p)
    this))

(defn update-element [this elem]
  (assoc this :elem elem))

(defn current-element [this]
  (:elem this))

(defn current-question [this p]
  (if-let [question (first (:quector (current-strategy this)))]
    ((:f question) this p)
    this))

(defn- next-question [state index]
  (update-in (:strategies state) [index :quector] #(vec (take (count %)(rest (cycle %))))))

(defn current-dialog [this]
  (:strategies this))



;; shape-specific functions
;;
(defprotocol IShapesFactory
  (create[this])
  (finish[this]))


;;
;; point factory
;;
(defrecord PointFactory[elem] IShapesFactory
  (create [this]
    (let [strategies [{:s "draw point"
                       :f (fn[state p] (change-strategy state "draw point"))
                       :highlight true
                       :quector [{:f (fn[this p]
                                       (let [pnt (current-element this)]
                                         (assoc this :elem (assoc pnt :p p :p-ref p)
                                                     :complete? true)))
                                  :g (fn[this p]
                                       (let [pnt (assoc (current-element this) :p p :p-ref p)]
                                         (update-element this pnt)))}]}
                      {:s "draw pin point"
                       :f (fn[state p] (change-strategy state "draw point"))
                       :highlight false
                       :quector [{:f (fn[this p]
                                       (let [pnt (current-element this)]
                                         (assoc this :elem (assoc pnt :p p :p-ref p)
                                                     :complete? true)))
                                  :g (fn[this p]
                                       (let [pnt (assoc (current-element this) :p p :p-ref p)]
                                         (update-element this pnt)))}]}]]

      (assoc this :elem elem
                  :complete? false
                  :strategies strategies)))

  (finish [this]
    (elements/update-elements [:create (current-element this)])
    [:point (current-element this)]))


(defn- next-point [p line]
  (let [q (math/coordinates p)]
    (if (:ortho-polyline? line)
      (let [p1 [(first (:p1 line)) (second q)]
            p2 [(first q) (second (:p1 line))]]
        (if (< (math/dist q p1)(math/dist q p2)) p1 p2))
      q)))


(defrecord LineFactory[elem] IShapesFactory
  (create [this]
    (let [strategies [{:s "polygone"
                       :f (fn[state p] (change-strategy state "polygone"))
                       :highlight (not (:ortho-polyline? elem))
                       :quector [{:f (fn[this p]
                                       (let [line (assoc (current-element this) :ortho-polyline? false)]
                                         (if (math/nearly-zero? (math/dist (:p1 line) p))
                                           this
                                           (assoc this
                                             :elem (assoc line :p2 p :p-ref (:p2 line))
                                             :complete? true))))
                                  :g (fn [this p]
                                       (let [line (assoc (current-element this) :ortho-polyline? false)
                                             p3 (next-point p line)]
                                         (update-element this (assoc line :p2 p3 :p-ref p3))))}]}
                      {:s "orthogonal mode"
                       :f (fn[state p] (change-strategy state "orthogonal mode"))
                       :highlight (:ortho-polyline? elem)
                       :quector [{ :f (fn[this p]
                                       (let [line (assoc (current-element this) :ortho-polyline? true)]
                                         (if (math/nearly-zero? (math/dist (:p1 line) p))
                                           this
                                           (assoc this
                                             :elem (assoc line :p2 (next-point p line) :p-ref (:p2 line))
                                             :complete? true))))
                                  :g (fn [this p]
                                       (let [line (assoc (current-element this) :ortho-polyline? true)
                                             p3 (next-point p line)]
                                         (update-element this (assoc line :p2 p3 :p-ref p3))))}]}]]
      (assoc this :elem elem
                  :complete? false
                  :strategies strategies)))


  (finish [this]
    (elements/update-elements [:create (current-element this)])
    [:line (current-element this)]))


;;
;; circle factory
;;
(defrecord CircleFactory[elem] IShapesFactory
  (create [this]
    (let [strategies [{:s "define center point & radius"
                       :f (fn[state p]
                            (change-strategy state "define center point & radius"))
                       :highlight true
                       :quector [{:f (fn[this p]
                                       (if-let [circle (current-element this)]
                                         (assoc this
                                           :elem (assoc circle :p-center p :p-ref p)
                                           :strategies (next-question this 0))
                                         this))
                                  :g (fn[this p]
                                       (->> (math/vec-sub p (:p-center (current-element this)))
                                            (shapes/translate (current-element this))
                                            (update-element this)))}
                                 {:f (fn[this p]
                                       (if-let [circle (current-element this)]
                                         (assoc this
                                           :elem (assoc circle :radius (math/dist (:p-center circle) p)
                                                               :p-ref (if (math/equals? (:p-center circle) p)
                                                                        (math/vec-add
                                                                          p
                                                                          [(math/dist (:p-center circle) p)
                                                                           (second (:p-center circle))])
                                                                        (math/project-circle
                                                                          p
                                                                          (:p-center circle)
                                                                          (:radius circle))))
                                           :complete? true)
                                         this))
                                  :g (fn[this p]
                                       (let [factor (/ (math/dist (:p-center (current-element this)) p)
                                                       (max (:radius (current-element this)) math/EPS))
                                             circle (shapes/scale (current-element this) factor)]
                                         (if-not (math/equals? p (:p-center (current-element this)))
                                           (update-element this circle)
                                           this)))}]}
                      {:s "circumcircle (three points)"
                       :f (fn[state p]
                            (change-strategy state "circumcircle (three points)"))
                       :highlight false
                       :quector [{:f (fn[this [x y]]
                                       (if-let [circle (current-element this)]
                                         (assoc this
                                           :elem (assoc circle :p-center [x (- y (:radius circle))] :p-ref [x y])
                                           :strategies (next-question this 1))
                                         this))
                                  :g (fn[this p]
                                       (let [circle (current-element this)]
                                         (->> (math/vec-sub p (:p-center circle))
                                              (math/vec-add [0.0 (:radius circle)])
                                              (shapes/translate circle)
                                              (update-element this))))}
                                 {:f (fn[this p]
                                       (if-let [circle (current-element this)]
                                         (let [center (math/vec-scale (:p-ref circle) p 0.5)
                                               radius (math/dist center p)]
                                           (assoc this
                                             :elem (assoc circle :p-center center :radius radius :p-aux p)
                                             :strategies (next-question this 1)))
                                         this))
                                  :g (fn[this p]
                                       (let [circle (current-element this)
                                             center (math/vec-scale (:p-ref circle) p 0.5)
                                             radius (math/dist center p)]
                                         (update-element this (assoc circle :p-center center :radius radius))))}
                                 {:f (fn[this p]
                                       (let [circle (current-element this)
                                             [center radius] (math/circumcircle (:p-ref circle) (:p-aux circle) p)]
                                         (assoc this
                                           :elem (assoc (dissoc circle :p-aux) :p-center center :radius radius :p-ref center)
                                           :complete? true)))
                                  :g (fn[this p]
                                       (let [circle (current-element this)
                                             [center radius] (math/circumcircle (:p-ref circle) (:p-aux circle) p)]
                                         (update-element this (assoc circle :p-center center :radius radius))))}]}]]
      (assoc this :elem elem
                  :complete? false
                  :strategies strategies)))


  (finish [this]
    (elements/update-elements [:create (current-element this)])
    [:circle (current-element this)]))



;;
;; arc factory
;;
(defrecord ArcFactory[elem] IShapesFactory
  (create [this]
    (let [strategies [{:s "three points (start, end, plus one)"
                       :f (fn[state p] (change-strategy state "start, end, on points"))
                       :highlight true
                       :quector [{:f (fn[this p]
                                       (if-let [arc (current-element this)]
                                         (assoc this
                                           :elem (assoc arc :p-start p :p-ref p)
                                           :strategies (next-question this 0))
                                         this))
                                  :g (fn[this p]
                                       (->> (math/vec-sub p (:p-start (current-element this)))
                                            (shapes/translate (current-element this))
                                            (update-element this)))}
                                 {:f (fn[this p]
                                       (if-let [arc (current-element this)]
                                         (assoc this
                                           :elem (assoc arc :p-end p :p-ref p)
                                          :strategies (next-question this 0))
                                         this))
                                  :g (fn[this p]
                                       (let [arc (current-element this)
                                             factor (/ (math/dist (:p-start arc) p)
                                                       (math/dist (:p-start arc) (:p-end arc)))
                                             new-p-c  (math/vec-scale (:p-start arc) (:p-center arc) factor)]
                                         (if-not (math/nearly-zero? factor)
                                           (update-element this (shapes/constructArc
                                                                new-p-c
                                                                (math/dist (:p-start arc) new-p-c)
                                                                (:p-start arc)
                                                                p))
                                           this)))}
                                 {:f (fn[this p]
                                       (let [circle (math/circumcircle
                                                      (:p-start (current-element this))
                                                      p
                                                      (:p-end (current-element this)))]
                                         (if-let [arc (assoc (current-element this) :p-center (first circle)
                                                                                    :radius (second circle))]
                                           (assoc this
                                             :complete? true
                                             :elem arc
                                             :p-ref (math/project-point-onto-circle p (first circle)(second circle))
                                             :strategies (next-question this 0))
                                           this)))
                                  :g (fn[this p]
                                       (let [p-s (:p-start (current-element this))
                                             p-e (:p-end (current-element this))
                                             circle (math/circumcircle p-s p p-e)]
                                         (if-not (nil? circle)
                                           (update-element this (shapes/constructArc
                                                               (first circle)
                                                               (second circle)
                                                               p-s
                                                               p-e))
                                           this)))}]}
                       {:s "center, start, end points"
                        :f (fn[state p] (change-strategy state "center, start, end points"))
                        :highlight false
                        :quector [{:f (fn[this p]
(prn "3 point arc definition, start point")
                                        (if-let [arc (current-element this)]
                                          (assoc this
;;                                            :elem (assoc arc :p-start p :p-ref p)
                                           :strategies (next-question this 1)))
                                          this)
                                  :g (fn[this p]
                                       this)}
                                 {:f (fn[this p]
(prn "3 point arc definition, end point")
                                        (if-let [arc (current-element this)]
                                          (assoc this
;;                                            :elem (assoc arc :p-start p :p-ref p)
                                           :strategies (next-question this 1))
                                          this))
                                  :g (fn[this p]
                                       this)}
                                 {:f (fn[this p]
(prn "3 point arc definition, 3rd point")
                                        (if-let [arc (current-element this)]
                                          (assoc this
;;                                            :elem (assoc arc :p-start p :p-ref p)
                                            :complete? true
                                            this)))
                                  :g (fn[this p]
                                       this)}]}]]
      (assoc this :elem elem
                  :complete? false
                  :strategies strategies)))


  (finish [this]
    (elements/update-elements [:create (current-element this)])
    [:arc (current-element this)]))




;;
;; contour factory
;;
(defrecord ContourFactory[elem] IShapesFactory
  (create [this]
    (let [strategies [{:s "pick a trace"
                       :f (fn[state p] (change-strategy state "pick a trace"))
                       :highlight true
                       :quector [{:f (fn[this p]
                                       (if-let [contour (current-element this)]
                                         (assoc this
                                           :complete? true
                                           :elem (assoc contour :p-list (reverse (:p-list contour))
                                                                :p-ref (last (:p-list contour))))
                                         this))
                                  :g (fn[this p]
                                       (assoc this :elem (shapes/translate
                                                          (current-element this)
                                                          (math/vec-sub p (:p-ref (current-element this))))))}]}
                      {:s "spline"
                       :f (fn[state p] (change-strategy state "spline"))
                       :highlight false
                       :quector [{:f (fn[this p]
                                       (if-let [contour (current-element this)]
                                         (assoc this
                                           :complete? true
                                           :elem (assoc contour :p-list (reverse (:p-list contour))
                                                                :p-ref (last (:p-list contour))))
                                         this))
                                  :g (fn[this p]
                                       (update-element this (shapes/translate
                                                            (current-element this)
                                                            (math/vec-sub p (:p-ref (current-element this))))))}]}]]

      (assoc this :elem elem
                  :complete? false
                  :strategies strategies)))


  (finish [this]
    (elements/update-elements [:create (current-element this)])
    [:contour (current-element this)]))



(defn createShapeFactory [elem]
  (case (:type elem)
    :point   (create (->PointFactory elem))
    :line    (create (->LineFactory elem))
    :circle  (create (->CircleFactory elem))
    :arc     (create (->ArcFactory elem))
    :contour (create (->ContourFactory elem))
             {:type :none :elem elem}))

