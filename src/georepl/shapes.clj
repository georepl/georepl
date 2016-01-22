(ns georepl.shapes
  (:require [georepl.mathlib :as math]))



(defprotocol IShape
  (construct [this])
  (translate[this v])
  (rotate[this angle])
  (scale[this factor]))


;; 'point' basic shape
;;
(defrecord Point [p] IShape
  (construct [this]
    (-> this
      (assoc :type :point
             :visible 1
             :p-ref p
             :p p)))

  (translate [this v]
    (let [q (math/add-vec (:p this) v)]
      (assoc this :p q
                  :p-ref q)))

  (rotate [this angle] this)

  (scale [this factor] this))


(defn constructPoint [p]
  (construct (->Point p)))




;; 'line' basic shape
;;
(defrecord Line [p1 p2] IShape
  (construct [this]
    (-> this
      (assoc :type :line
             :visible 1
             :p-ref p1
             :p1 p1
             :p2 p2)))

  (translate [this v]
    (let [q (math/add-vec p1 v)]
      (assoc this :p1 q
                  :p2 (math/add-vec p2 v)
                  :p-ref q)))

  (rotate [this angle] this)

  (scale [this factor] this))


(defn constructLine [p1 p2]
  (construct (->Line p1 p2)))



;; 'circle' basic shape
;;
(defrecord Circle [p-center radius] IShape
  (construct [this]
    (-> this
      (assoc :type :circle
             :visible 1
             :p-ref p-center
             :p-center p-center
             :radius radius)))

  (translate [this v]
    (let [q (math/add-vec p-center v)]
      (assoc this :p-center q
                  :p-ref q)))

  (rotate [this angle] this)

  (scale [this factor]
;(println "SHAPES/SCALE:" factor)
    (assoc this :radius (* (:radius this) factor))))


(defn constructCircle [p-center radius]
  (construct (->Circle p-center radius)))



;; 'arc' basic shape
;;
(defrecord Arc [p-center radius p-start p-end] IShape
  (construct [this]
    (-> this
      (assoc :type :arc
             :visible 1
             :p-ref p-center
             :p-center p-center
             :radius radius
             :p-start p-start
             :p-end p-end)))

  (translate [this v]
    (-> this
      (assoc :p-center (math/add-vec p-center v)
             :p-start (math/add-vec p-start v)
             :p-end (math/add-vec p-end v)
             :p-ref (math/add-vec (:p-ref this) v))))

  (rotate [this angle]
    (-> this
      (assoc :p-start (math/rotate p-center p-start  angle)
             :p-end (math/rotate p-center p-end angle)
             :p-ref (math/rotate p-center (:p-ref this) angle))))

  (scale [this factor]
    (assoc this :radius (* radius factor)
                :p-start (math/scale-vec p-center p-start factor)
                :p-end (math/scale-vec p-center p-end factor))))


(defn constructArc [p-center radius p-start p-end]
  (construct (->Arc p-center radius p-start p-end)))



;; 'contour' basic shape
;;
(defrecord Contour [p-list] IShape
  (construct [this]
    (-> this
      (assoc :type :contour
             :visible 1
             :p-ref (first p-list)
             :p-list (vec p-list))))

  (translate [this v]
    (let [new-p-list (vec (map (partial math/add-vec v) p-list))]
      (assoc this :p-list new-p-list
                  :p-ref (first new-p-list))))

  (rotate [this angle]
    (let [p-center (* 0.5
                      (math/difference (first p-list)
                                       (last p-list)))]
      (assoc this :p-list
                  (vec (map math/rotate p-center p-list)))))

  (scale [this factor]
    (assoc this :p-list
                (vec (map (map (partial * factor)) p-list)))))

(defn constructContour [p-list]
  (construct (->Contour p-list)))
