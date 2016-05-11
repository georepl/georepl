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
                                  (assoc this :elem (assoc pnt :p p :p-ref p)
                                              :back-to-drawing true
                                              :complete? true)))
                           :g (fn[this p]
                                (let [pnt (assoc (current-element this) :p p :p-ref p)]
                                  (update-element this pnt)))
                           :highlight 1}]))


  (finish [this]
    (elements/push-elem
      (assoc elem :name (elements/unique-name "Pnt")))
    :point))




;;
;; line factory
;;
(defrecord LineFactory[elem] IShapesFactory
  (create [this]
    (assoc this :elem elem
                :complete? false
                :just-answered :none
                :quector [{:s "next point"
                           :f (fn[this p]
                                (let [line (current-element this)]
                                  (if (math/nearly-zero? (math/dist (:p1 line) p))
                                    this
                                    (assoc this :elem (assoc line :p2 p
                                                                  :p-ref (:p2 line))
                                                :complete? true))))
                           :g (fn [this p]
                                (let [line (assoc (current-element this) :p2 p :p-ref p)]
                                  (update-element this line)))
                           :highlight 1}
                          {:s "return"
                           :f (fn[this p]
                                (assoc this :complete? true
                                            :back-to-drawing true))
                           :g (fn [this p]
                                (let [line (assoc (current-element this) :p2 p :p-ref p)]
                                  (update-element this line)))
                           :highlight 0}]))


  (finish [this]
    (elements/push-elem
      (assoc (current-element this) :name (elements/unique-name "Ln")))
    :line))


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
                               (assoc this :elem (assoc circle :p-center p
                                                               :p-ref p)
                                           :quector (rest (:quector this))
                                           :complete? (= (:just-answered this) :radius)
                                           :just-answered :center)
                               nil))
                        :g (fn[this p]
                             (->> (math/vec-sub p (:p-center (current-element this)))
                                  (shapes/translate (current-element this))
                                  (update-element this)))
                        :highlight 1}
                       {:s "define radius"
                        :f (fn[this p]
                             (if-let [circle (current-element this)]
                               (assoc this :elem (assoc circle :radius (math/dist (:p-center circle) p)
                                                               :p-ref (if (math/equals? (:p-center circle) p)
                                                                        (math/vec-add p [(math/dist (:p-center circle) p)
                                                                                         (second (:p-center circle))])
                                                                        (math/project-circle
                                                                          p
                                                                          (:p-center circle)
                                                                          (:radius circle))))
                                           :quector (rest (:quector this))
                                           :complete? (= (:just-answered this) :center)
                                           :just-answered :radius)
                               nil))
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
                             (assoc-in this [:elem :complete?] true))
                        :g (fn[this p]
                             this)
                        :highlight 0}])))


  (finish [this]
    (elements/push-elem
      (assoc (current-element this) :name (elements/unique-name "Cir")))
    :circle))




;;
;; arc factory
;;
(defrecord ArcFactory[elem] IShapesFactory
  (create [this]
    (-> this
      (assoc :elem  elem
             :complete? false
             :quector [{:s "pick start point"
                        :f (fn[this p]
                             (if-let [arc (current-element this)]
                               (assoc this :elem (assoc arc :p-start p
                                                            :p-ref p)
                                           :quector (rest (:quector this))
                                           :complete? false
                                           :just-answered :start)
                               nil))
                        :g (fn[this p]
                             (->> (math/vec-sub p (:p-start (current-element this)))
                                  (shapes/translate (current-element this))
                                  (update-element this)))
                        :highlight 1}
                       {:s "pick end point"
                        :f (fn[this p]
                             (if-let [arc (current-element this)]
                               (assoc this :elem (assoc arc :p-end p
                                                            :p-ref p)
                                           :quector (rest (:quector this))
                                           :complete? false
                                           :just-answered :end)
                               nil))
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
                                 (assoc this :elem arc
                                             :p-ref (math/project-point-onto-circle
                                                      p (first circle)(second circle))
                                             :quector (rest (:quector this))
                                             :complete? true
                                             :just-answered :onp)
                                 nil)))
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
                        :highlight 0}])))


  (finish [this]
    (elements/push-elem
      (assoc (current-element this) :name (elements/unique-name "Arc")))
    :arc))




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
                                (assoc this :elem (assoc contour :p-list (reverse (:p-list contour))
                                                                 :p-ref (last (:p-list contour))
                                                                 :complete? false)
                                            :complete? true)
                                this))
                        :g (fn[this p]
                             (assoc this :elem (shapes/translate
                                                (current-element this)
                                                (math/vec-sub p (:p-ref (current-element this))))))
                        :highlight 1}
                       {:s "ok?"
                        :f (fn[this p]
                             (assoc this :complete? true))
                        :g (fn[this p]
                             (update-element this (shapes/translate
                                                  (current-element this)
                                                  (math/vec-sub p (:p-ref (current-element this))))))
                        :highlight 0}])))


  (finish [this]
    (elements/push-elem
      (assoc (current-element this) :name (elements/unique-name "Con")))
    :contour))



(defn createShapeFactory [elem]
  (case (:type elem)
    :point   (create (->PointFactory elem))
    :line    (create (->LineFactory elem))
    :circle  (create (->CircleFactory elem))
    :arc     (create (->ArcFactory elem))
    :contour (create (->ContourFactory elem))
             {:type :none :elem elem}))

