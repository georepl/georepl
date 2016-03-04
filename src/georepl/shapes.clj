(ns georepl.shapes
  (:require [georepl.mathlib :as math]))



(defprotocol IShape
  (construct [this])
  (next-point [this p])
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

  (next-point [this p]
    [this (:p this) (math/dist p (:p this))])

  (translate [this v]
    (let [q (math/vec-add (:p this) v)]
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

  (next-point [this p]
    (let [l1 (math/dist p (:p1 this))
          l2 (math/dist p (:p2 this))]
      (if (< l1 l2)
        [this (:p1 this) l1]
        [this (:p2 this) l2])))

  (translate [this v]
    (let [q (math/vec-add p1 v)]
      (assoc this :p1 q
                  :p2 (math/vec-add p2 v)
                  :p-ref q)))

  (rotate [this angle] this)

  (scale [this factor] this))


(defn constructLine
  ([p1 p2]
    (construct (->Line p1 p2)))
  ([p1 p2 visible]
    (let [line (construct (->Line p1 p2))]
      (assoc line :visible visible))))



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

  (next-point [this p]
    (let [l1 (math/dist p (:p-center this))
          q  (math/project-point-onto-circle p p-center radius)
          l2 (math/dist p q)]
      (if (< l1 l2)
        [this (:p-center this) l1]
        [this q l2])))

  (translate [this v]
    (let [q (math/vec-add p-center v)]
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
             :p-center p-center
             :radius radius
             :p-start p-start
             :p-end p-end
             :p-ref p-start)))


  (next-point [this p]
    (let [q  (math/project-point-onto-circle p p-center radius)
          c1 [(:p-center this)(:p-start this)(:p-end this)]
          c2 (if (math/right-from? q p-start p-end)
               (cons q c1)
               c1)
          nxt (first (sort-by (partial math/dist p) c2))]
      [this nxt (math/dist p nxt)]))


  (translate [this v]
    (-> this
      (assoc :p-center (math/vec-add p-center v)
             :p-start (math/vec-add p-start v)
             :p-end (math/vec-add p-end v)
             :p-ref (math/vec-add (:p-ref this) v))))

  (rotate [this angle]
    (-> this
      (assoc :p-start (math/rotate p-center p-start  angle)
             :p-end (math/rotate p-center p-end angle)
             :p-ref (math/rotate p-center (:p-ref this) angle))))

  (scale [this factor]
    (assoc this :radius (* radius factor)
                :p-start (math/vec-scale p-center p-start factor)
                :p-end (math/vec-scale p-center p-end factor))))


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

  (next-point [this p]
    (first
      (sort #(compare (last %1)(last %2))
        (map #(vec (list this % (math/dist p %))) (:p-list this)))))

  (translate [this v]
    (let [new-p-list (vec (map (partial math/vec-add v) p-list))]
      (assoc this :p-list new-p-list
                  :p-ref (first new-p-list))))

  (rotate [this angle]
    (let [p-center (* 0.5
                      (math/vec-sub (first p-list)
                                       (last p-list)))]
      (assoc this :p-list
                  (vec (map math/rotate p-center p-list)))))

  (scale [this factor]
    (assoc this :p-list
                (vec (map (map (partial * factor)) p-list)))))

(defn constructContour [p-list]
  (construct (->Contour p-list)))
