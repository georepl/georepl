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
                       (math/difference (:p-ref (:elem this)) p))))

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
;;                         (math/difference (:p-ref (:elem this)) p))]
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
                             (let [v (math/difference (:p-center (:elem this)) p)
                                   circle (shapes/translate (:elem this) v)]
                               (assoc this :elem circle)))}
                       {:s "define radius"
                        :f (fn[this p]
                             (if-let [circle (:elem this)]
                               (assoc this :elem (assoc circle :radius (math/dist (:p-center circle) p)
                                                               :p-ref (if (math/equals? (:p-center circle) p)
                                                                        (math/add-vec p [(math/dist (:p-center circle) p)
                                                                                         (second (:p-center circle))])
                                                                        (math/project p
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
                             (let [v (math/difference (:p-center (:elem this)) p)
                                   circle (shapes/translate (:elem this) v)]
                               (assoc this :elem circle)))}
                       {:s "define radius"
                        :f (fn[this p]
                             (if-let [circle (:elem this)]
                               (assoc this :elem (assoc circle :radius (math/dist (:p-center circle) p)
                                                               :p-ref (if (math/equals? (:p-center circle) p)
                                                                        (math/add-vec p [(math/dist (:p-center circle) p)
                                                                                         (second (:p-center circle))])
                                                                        (math/project p
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
    (let [e (:elem this)]
      (case (:just-answered this)
        :radius  (let [v (math/difference (:p-ref e) p)]
                   (assoc this :elem (shapes/translate e v)))
        :center  (let [factor (/ (math/dist (:p-center e) p)
                                 (max (:radius e) math/EPS))]
                   (assoc this :elem (shapes/scale e factor)))
                 this)))

  (update-element [this elem]
    (assoc this :elem elem))

  (current-element [this]
    (:elem this))

  (current-question[this]
    [{:s "pick center point"
      :f (fn[this p]
           (if-let [arc (:elem this)]
             (assoc this :elem (assoc arc :p-center p
                                          :p-ref p
                                          :complete? false)
                         :just-answered   :center)
             this))
      :type :next-input
      :val 0}
     {:s "pick a start point"
      :f (fn[this p]
           (if-let [arc (:elem this)]
             (assoc this :elem (assoc arc :p-start (math/project p (:p-center this) (:radius this))
                                          :p-ref p
                                          :complete? false)
                         :just-answered   :start)
            this))
      :type :next-input
      :val 0}
     {:s "pick an end point"
      :f (fn[this p]
           (if-let [arc (:elem this)]
             (assoc this :elem (assoc arc :p-end (math/project p (:p-center this) (:radius this))
                                          :p-ref p
                                          :complete? false)
                         :just-answered   :end))
             this)
      :type :next-input
      :val 0}
     {:s "define radius"
      :f (fn[this p]
           (if-let [circle (:elem this)]
             (assoc this :elem (assoc circle :radius (math/dist (:p-center this) p)
                                             :p-ref p
                                             :complete? false)
                         :just-answered :radius)
            this))
      :type :next-input
      :val 0}
     {:s "ok?"
      :f (fn[this p]
           (assoc this :complete? true
                       :just-answered :none))
      :type :immediate
      :val 1}])

  (finish [this]
    (elements/push-elem (:elem this))
    :arc))




;;
;; contour factory
;;
(defrecord ContourFactory[elem] IShapesFactory
  (create [this]
    (assoc this :elem elem
                :complete? false))

  (refresh [this p]
    (assoc this :elem (shapes/translate
                       (:elem this)
                       (math/difference (:p-ref (:elem this)) p))))

  (update-element [this elem]
    (assoc this :elem elem))

  (current-element [this]
    (:elem this))

  (current-question[this]
    [{:s "toggle reference point"
      :f (fn[this p]
           (if-let [contour (:elem this)]
             (assoc this :elem (assoc contour :p-list (reverse (:p-list contour))
                                              :p-ref (last (:p-list contour))
                                              :complete? false))
             this))
      :type :immediate
      :val 0}
     {:s "ok?"
      :f (fn[this p](assoc this :complete? true))
      :type :immediate
      :val 1}])

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

