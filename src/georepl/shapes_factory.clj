(ns georepl.shapes-factory
  (:require [clojure.core.match :refer [match]]
            [georepl.shapes :as shapes]
            [georepl.elements :as elements]
            [georepl.mathlib :as math]))


;; functions which are identical for all shapes
;;
(defn refresh [this p]
  (if-let [question (first (:quector this))]
    ((:g question) this p)
    this))

(defn update-element [this elem]
  (assoc this :elem elem))

(defn current-element [this]
  (:elem this))

(defn current-question [this]
  (if-let [question (first (:quector this))]
    (:f question)
    nil))

(defn current-dialog [this]
  (:quector this))



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
    (assoc this :elem elem
                :complete? false
                :just-answered :none
                :quector [{:s "return"
                           :f (fn[this p]
                                (let [pnt (current-element this)]
                                  (assoc (assoc this :complete? true)
                                    :elem (assoc pnt :p p :p-ref p)
                                    :back-to-drawing? true)))
                           :g (fn[this p]
                                (let [pnt (assoc (current-element this) :p p :p-ref p)]
                                  (update-element this pnt)))
                           :highlight 1}]))


  (finish [this]
    (elements/update-elements [:create (current-element this)])
    [:point (current-element this)]))


(defn- next-point [p line]
  (let [q (take 2 p)]
    (if (:ortho-polyline? line)
      (let [p1 [(first (:p1 line)) (second q)]
            p2 [(first q) (second (:p1 line))]]
        (if (< (math/dist q p1)(math/dist q p2)) p1 p2))
      q)))


(defrecord LineFactory[elem] IShapesFactory
  (create [this]
    (assoc this :elem elem
                :complete? false
                :just-answered :none
                :quector [{:s "toggle point"
                           :f (fn[this p]
                               (let [line (current-element this)]
                                 (if (math/nearly-zero? (math/dist (:p1 line) p))
                                   this
                                   (assoc this
                                     :elem (assoc line :p2 (next-point p line) :p-ref (:p2 line))
                                     :complete? true))))
                           :g (fn [this p]
                                (let [line (current-element this)
                                      p3 (next-point p line)]
                                  (update-element this (assoc line :p2 p3 :p-ref p3))))
                           :highlight 1}
                          {:s "return"
                           :f (fn[this p]
                                (assoc this :complete? true :back-to-drawing? true))
                           :g (fn [this p]
                                (let [line (current-element this)
                                      p3 (next-point p line)]
                                  (update-element this (assoc line :p2 p3 :p-ref p3))))
                           :highlight 0}]))


  (finish [this]
    (elements/update-elements [:create (current-element this)])
    [:line (current-element this)]))


;;
;; circle factory
;;
(defrecord CircleFactory[elem] IShapesFactory
  (create [this]
    (-> this
      (assoc :elem  elem
             :complete? false
             :quector [{:s "pick center point"
                        :f (fn[this p]
                             (if-let [circle (current-element this)]
                               (assoc this
                                 :complete? (= (:just-answered this) :radius)
                                 :elem (assoc circle :p-center p :p-ref p)
                                 :quector (rest (:quector this))
                                 :just-answered :center)
                               this))
                        :g (fn[this p]
                             (->> (math/vec-sub p (:p-center (current-element this)))
                                  (shapes/translate (current-element this))
                                  (update-element this)))
                        :highlight 1}
                       {:s "define radius"
                        :f (fn[this p]
                             (if-let [circle (current-element this)]
                               (assoc this
                                 :complete? (= (:just-answered this) :center)
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
                                           :quector (rest (:quector this))
                                           :just-answered :radius)
                               this))
                        :g (fn[this p]
                             (let [factor (/ (math/dist (:p-center (current-element this)) p)
                                             (max (:radius (current-element this)) math/EPS))
                                   circle (shapes/scale (current-element this) factor)]
                               (if-not (math/equals? p (:p-center (current-element this)))
                                 (update-element this circle)
                                 this)))
                        :highlight 0}
                       {:s "define point on circle"
                        :f (fn[this p]
                             (assoc this :complete? false))
                        :g (fn[this p]
                             this)
                        :highlight 0}
                       {:s "return"
                           :f (fn[this p]
                                (let [pnt (current-element this)]
                                  (assoc this
                                    :complete? true
                                    :elem (assoc pnt :p p :p-ref p)
                                    :back-to-drawing? true)))
                           :g (fn[this p]
                                (let [pnt (assoc (current-element this) :p p :p-ref p)]
                                  (update-element this pnt)))
                           :highlight 0}])))


  (finish [this]
    (elements/update-elements [:create (current-element this)])
    [:circle (current-element this)]))



;;
;; arc factory
;;
(defrecord ArcFactory[elem] IShapesFactory
  (create [this]
    (-> this
      (assoc :elem  elem
             :quector [{:s "pick start point"
                        :f (fn[this p]
                             (if-let [arc (current-element this)]
                               (assoc this
                                 :complete? false
                                 :elem (assoc arc :p-start p :p-ref p)
                                 :quector (rest (:quector this))
                                 :just-answered :start)
                               this))
                        :g (fn[this p]
                             (->> (math/vec-sub p (:p-start (current-element this)))
                                  (shapes/translate (current-element this))
                                  (update-element this)))
                        :highlight 1}
                       {:s "pick end point"
                        :f (fn[this p]
                             (if-let [arc (current-element this)]
                               (assoc this
                                 :complete? false
                                 :elem (assoc arc :p-end p :p-ref p)
                                 :quector (rest (:quector this))
                                 :just-answered :end)
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
                                 this)))
                        :highlight 0}
                       {:s "define point on circle"
                        :f (fn[this p]
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
                                   :quector (rest (:quector this))
                                   :just-answered :onp)
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
                                 this)))
                        :highlight 0}
                       {:s "return"
                           :f (fn[this p]
                                (let [pnt (current-element this)]
                                  (assoc this
                                    :complete? true
                                    :elem (assoc pnt :p p :p-ref p)
                                    :back-to-drawing? true)))
                           :g (fn[this p]
                                (let [pnt (assoc (current-element this) :p p :p-ref p)]
                                  (update-element this pnt)))
                           :highlight 0}])))


  (finish [this]
    (elements/update-elements [:create (current-element this)])
    [:arc (current-element this)]))




;;
;; contour factory
;;
(defrecord ContourFactory[elem] IShapesFactory
  (create [this]
    (-> this
      (assoc :elem  elem
             :complete? false
             :quector [{:s "pick a trace"
                        :f (fn[this p]
                              (if-let [contour (current-element this)]
                                (assoc this
                                  :complete? true
                                  :elem (assoc contour :p-list (reverse (:p-list contour))
                                                       :p-ref (last (:p-list contour))))
                                this))
                        :g (fn[this p]
                             (assoc this :elem (shapes/translate
                                                (current-element this)
                                                (math/vec-sub p (:p-ref (current-element this))))))
                        :highlight 1}
                       {:s "return"
                        :f (fn[this p]
                             (assoc this :complete? true))
                        :g (fn[this p]
                             (update-element this (shapes/translate
                                                  (current-element this)
                                                  (math/vec-sub p (:p-ref (current-element this))))))
                        :highlight 0}])))


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

