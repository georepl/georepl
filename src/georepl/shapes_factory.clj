(ns georepl.shapes-factory
  (:require [clojure.core.match :refer [match]]
            [georepl.shapes :as shapes]
            [georepl.elements :as elements]
            [georepl.mathlib :as math]))



(defprotocol IShapesFactory
  (create[this])
  (refresh[this elem])
  (update-element[this elem])
  (current-element[this])
  (current-question[this])
  (finish[this]))


;;
;; point factory
;;
(defrecord PointFactory[elem] IShapesFactory
  (create [this]
    (elements/push-elem elem)
    (assoc this :complete? false))

  (refresh [this p]
    (assoc this :elem (shapes/translate
                       (:elem this)
                       (math/vec-sub p (:p-ref (:elem this))))))

  (current-element [this]
    (:elem this))

  (update-element [this elem]
    (assoc this :elem elem))

  (current-question[this]
    [{:s "single points"
      :f (fn[this p] (fn[this p] :point)
             this)
      :type :immediate
      :val 0}
     {:s "polygone"
      :f (fn[this p] :polygone)
      :type :immediate
      :val 1}])

  (finish [this]
    (elements/push-elem elem)
    :point))




;;
;; line factory
;;
(defrecord LineFactory[elem] IShapesFactory
  (create [this]
    (assoc this :elem elem
                :complete? false
                :just-answered :none
                :quector [{:s "toggle reference point"
                           :f (fn[this p]
                                (let [line (:elem this)]
                                  (if (math/nearly-zero? (math/dist (:p1 line) p))
                                    this
                                    (assoc this :elem (assoc line :p2 p
                                                                  :p-ref (:p2 line))
                                                :complete? true))))
                           :g (fn [this p]
                                (let [line (assoc (:elem this) :p2 p)]
                                  (assoc this :elem line)))}
                          {:s "ok?"
                           :f (fn[this p](assoc this :complete? true))
                           :g (fn [this p]
                                (let [line (assoc (:elem this) :p2 p)]
                                  (assoc this :elem line)))}]))

;;    (if (= (:just-asked this) :none)
;;      (let [line (shapes/translate
;;                         (:elem this)
;;                         (math/vec-sub p (:p-ref (:elem this))))]
;;        (assoc this :elem line))

  (refresh [this p]
    (if-let [quector (first (:quector this))]
      ((:g quector) this p)
      this))

  (update-element [this elem]
    (assoc this :elem elem))

  (current-element [this]
    (:elem this))

  (current-question[this]
    (if-let [quector (first (:quector this))]
      (:f quector)
      nil))

  (finish [this]
    (elements/push-elem (:elem this))
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
                             (if-let [circle (:elem this)]
                               (assoc this :elem (assoc circle :p-center p
                                                               :p-ref p)
                                           :quector (rest (:quector this))
                                           :complete? (= (:just-answered this) :radius)
                                           :just-answered :center)
                               nil))
                        :g (fn[this p]
                             (let [v (math/vec-sub p (:p-center (:elem this)))
                                   circle (shapes/translate (:elem this) v)]
                               (assoc this :elem circle)))}
                       {:s "define radius"
                        :f (fn[this p]
                             (if-let [circle (:elem this)]
                               (assoc this :elem (assoc circle :radius (math/dist (:p-center circle) p)
                                                               :p-ref (if (math/equals? (:p-center circle) p)
                                                                        (math/vec-add p [(math/dist (:p-center circle) p)
                                                                                         (second (:p-center circle))])
                                                                        (math/project-circle p
                                                                                      (:p-center circle)
                                                                                      (:radius circle))))
                                           :quector (rest (:quector this))
                                           :complete? (= (:just-answered this) :center)
                                           :just-answered :radius)
                               nil))
                        :g (fn[this p]
                             (let [factor (/ (math/dist (:p-center (:elem this)) p)
                                             (max (:radius (:elem this)) math/EPS))
                                   circle (shapes/scale (:elem this) factor)]
                               (if-not (math/equals? p (:p-center (:elem this)))
                                 (assoc this :elem circle)
                                 this)))}
                       {:s "define point on circle"
                        :f (fn[this p]
                             (assoc-in this [:elem :complete?] true))
                        :g (fn[this p]
                             this)}])))


  (refresh [this p]
    (if-let [quector (first (:quector this))]
      ((:g quector) this p)
      this))

  (update-element [this elem]
    (assoc this :elem elem))

  (current-element [this]
    (:elem this))

  (current-question[this]
    (if-let [quector (first (:quector this))]
      (:f quector)
      nil))

  (finish [this]
    (elements/push-elem (:elem this))
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
                             (if-let [arc (:elem this)]
                               (assoc this :elem (assoc arc :p-start p
                                                            :p-ref p)
                                           :quector (rest (:quector this))
                                           :complete? false
                                           :just-answered :start)
                               nil))
                        :g (fn[this p]
                             (let [v (math/vec-sub p (:p-start (:elem this)))
                                   arc (shapes/translate (:elem this) v)]
                               (assoc this :elem arc)))}
                       {:s "pick end point"
                        :f (fn[this p]
                             (if-let [arc (:elem this)]
                               (assoc this :elem (assoc arc :p-end p
                                                            :p-ref p)
                                           :quector (rest (:quector this))
                                           :complete? false
                                           :just-answered :end)
                               nil))
                        :g (fn[this p]
                             (let [p-s (:p-start (:elem this))
                                   p-e (:p-end (:elem this))
                                   p-c (:p-center (:elem this))
                                   factor (/ (math/dist p-s p)
                                             (math/dist p-s p-e))
                                   new-p-c  (math/vec-scale p-s p-c factor)]
                               (if-not (math/nearly-zero? factor)
                                 (assoc this :elem (shapes/constructArc
                                                     new-p-c
                                                     (math/dist p-s new-p-c)
                                                     p-s
                                                     p))
                                 this)))}
                       {:s "define point on circle"
                        :f (fn[this p]
                             (let [circle (math/circumcircle
                                            (:p-start (:elem this))
                                            p
                                            (:p-end (:elem this)))]
                               (if-let [arc (assoc (:elem this) :p-center (first circle)
                                                                :radius (second circle))]
                                 (assoc this :elem arc
                                             :p-ref (math/project-point-onto-circle
                                                      p (first circle)(second circle))
                                             :quector (rest (:quector this))
                                             :complete? true
                                             :just-answered :onp)
                                 nil)))
                        :g (fn[this p]
                             (let [p-s (:p-start (:elem this))
                                   p-e (:p-end (:elem this))
                                   circle (math/circumcircle p-s p p-e)]
                               (if-not (nil? circle)
                                 (assoc this :elem (shapes/constructArc
                                                    (first circle)
                                                    (second circle)
                                                    p-s
                                                    p-e))
                                 this)))}])))


  (refresh [this p]
    (if-let [quector (first (:quector this))]
      ((:g quector) this p)
      this))

  (update-element [this elem]
    (assoc this :elem elem))

  (current-element [this]
    (:elem this))

  (current-question[this]
    (if-let [quector (first (:quector this))]
      (:f quector)
      nil))

  (finish [this]
    (elements/push-elem (:elem this))
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
                              (if-let [contour (:elem this)]
                                (assoc this :elem (assoc contour :p-list (reverse (:p-list contour))
                                                                 :p-ref (last (:p-list contour))
                                                                 :complete? false)
                                            :complete? true)
                                this))
                        :g (fn[this p]
                             (assoc this :elem (shapes/translate
                                                (:elem this)
                                                (math/vec-sub p (:p-ref (:elem this))))))}
                       {:s "ok?"
                        :f (fn[this p]
                             (assoc this :complete? true))
                        :g (fn[this p]
                             (assoc this :elem (shapes/translate
                                                 (:elem this)
                                                 (math/vec-sub p (:p-ref (:elem this))))))}])))


  (refresh [this p]
    (if-let [quector (first (:quector this))]
      ((:g quector) this p)
      this))

  (update-element [this elem]
    (assoc this :elem elem))

  (current-element [this]
    (:elem this))

  (current-question[this]
    (if-let [quector (first (:quector this))]
      (:f quector)
      nil))

  (finish [this]
    (elements/push-elem (:elem this))
    :contour))




(defn createShapeFactory [elem]
  (case (:type elem)
    :point   (create (->PointFactory elem))
    :line    (create (->LineFactory elem))
    :circle  (create (->CircleFactory elem))
    :arc     (create (->ArcFactory elem))
    :contour (create (->ContourFactory elem))
             {:type :none :elem elem}))

