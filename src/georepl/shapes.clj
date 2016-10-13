(ns georepl.shapes
  (:require [georepl.mathlib :as math]))


(defprotocol IShape
  (construct [this])
  (next-point [this p])
  (translate[this v])
  (rotate[this angle])
  (rotate-ref[this p angle])
  (scale[this factor])
  (scale-ref[this p factor])
  (intersect [this shape])
  (between? [this q p1 p2])
  (points [this])
  (name-prefix [this])
  (sort-points [this pnt-list])
  (cut [this [p q]])
  (transform-points [this f])
)



;;
;; functions for all shapes
;;
(declare constructPoint constructLine constructCircle constructArc)

(defn on-element [p elem]
  (case (:type elem)
    :point  (if (math/equals? p (:p elem)) p nil)
    :line   (if (math/on-line? p (:p1 elem)(:p2 elem)) p nil)
    :circle (if (math/on-circle? p (:p-center elem)(:radius elem)) p nil)
    :arc    (if (math/on-arc? p (:p-center elem)(:radius elem)(:p-start elem)(:p-end elem)) p nil)
            nil))


;; 'point' basic shape
;;
(defrecord Point [p] IShape
  (construct [this]
    (-> this
      (assoc :type :point
             :visible 1
             :p-ref p)))

  (next-point [this p]
    [this (:p this) (math/dist p (:p this))])

  (translate [this v]
    (let [q (math/vec-add (:p this) v)]
      (assoc this :p q
                  :p-ref q)))

  (rotate [this angle]
    this)

  (rotate-ref [this p-r angle]
    (let [p (math/vec-rotate (:p this) p-r angle)]
      (assoc this :p p
                  :p-ref p)))

  (scale [this factor]
      this)

  (scale-ref [this p-r factor]
    (-> this
      (assoc :p (math/vec-scale p-r (:p this) factor)
             :p-ref (math/vec-scale p-r (:p-ref this) factor))))

  (intersect [this shape]
    (case (:type shape)
      :point  (if (math/equals? (:p this)(:p shape)) [(:p this)] [])
      :line   (if (math/on-line? (:p this)(:p1 shape)(:p2 shape)) [(:p this)] [])
      :circle (if (math/on-circle? (:p this)(:p-center shape)(:radius shape)) [(:p this)] [])
      :arc    (if (math/on-arc? (:p this)(:p-center shape)(:radius shape)(:p-start shape)(:p-end shape)) [(:p this)] [])
              []))

  (between? [this q p1 p2]
    (and (math/equals? p1 p2)
         (math/equals? q p2)
         (math/equals? q (:p this))))

  (points [this]
    [(:p this)])

  (name-prefix [this]
    "Pnt")

  (sort-points [this pnt-list]
    (dedupe
      (filter (partial math/equals? (:p this)) pnt-list)))

  (cut [this [p q]]
    [])

  (transform-points [this f]
    (assoc this :p (f (:p this)) :p-ref (f (:p-ref this)))))

(defn constructPoint [p]
  (construct (->Point p)))




;; 'line' basic shape
;;
(defrecord Line [p1 p2] IShape
  (construct [this]
    (-> this
      (assoc :type :line
             :visible 1
             :p-ref p1)))

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

  (rotate [this angle]
    (rotate-ref this (:p-ref this) angle))

  (rotate-ref [this p-r angle]
    (let [p (math/vec-rotate (:p1 this) p-r angle)
          q (math/vec-rotate (:p2 this) p-r angle)]
      (assoc this :p1 p :p2 q :p-ref p)))

  (scale [this factor]
    (scale-ref this (:p-ref this) factor))

  (scale-ref [this p-r factor]
    (assoc this :p1 (math/vec-scale p-r (:p1 this) factor)
                :p2 (math/vec-scale p-r (:p2 this) factor)
                :p-ref (math/vec-scale p-r (:p-ref this) factor)))

  (intersect [this shape]
    (case (:type shape)
      :point  (if (math/on-line? (:p shape)(:p1 this)(:p2 this)) [(:p shape)] [])
      :line   (math/intersect-lines (:p1 this)(:p2 this)(:p1 shape)(:p2 shape))
      :circle (math/intersect-line-circle (:p1 this)(:p2 this)(:p-center shape)(:radius shape))
      :arc    (math/intersect-line-arc (:p1 this) (:p2 this) (:p-center shape)(:radius shape)(:p-start shape)(:p-end shape))
              []))

  (between? [this q p1 p2]
    (math/on-line? q p1 p2))

  (points [this]
    [(:p1 this)(:p2 this)])

  (name-prefix [this]
    "Ln")

  (sort-points [this pnt-list]
    (->> pnt-list
         (filter #(math/on-line? % (:p1 this)(:p2 this)))
         (dedupe)
         (sort-by (partial math/dist (:p1 this)))))

  (cut [this [p q]]
    (if (math/equals? (:p1 this) p)
      (if (math/equals? (:p2 this) q)
        [ :delete this ]
        [ :delete this :create (assoc this :p1 q :p-ref q :name (:name this)) ])
      (if (math/equals? (:p2 this) q)
        [ :delete this :create (assoc this :p2 p :name (:name this)) ]
        [ :delete this :create (assoc this :p2 p :name (:name this))
          :create (constructLine q (:p2 this))])))

  (transform-points [this f]
    (assoc this :p1 (f (:p1 this)) :p2 (f (:p2 this)) :p-ref (f (:p-ref this)))))


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
             :p-ref p-center)))

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

  (rotate [this angle]
    this)

  (rotate-ref [this p-r angle]
    (let [p (math/vec-rotate (:p-center this) p-r angle)]
      (assoc this :p-center p
                  :p-ref p)))

  (scale [this factor]
    (assoc this :radius (* (:radius this) factor)))

  (scale-ref [this p-r factor]
    (assoc (scale this factor) :p-center (math/vec-scale p-r (:p-center this) factor)
                               :p-ref (math/vec-scale p-r (:p-ref this) factor)))

  (intersect [this shape]
    (case (:type shape)
      :point (if (math/on-circle? (:p shape)(:p-center this)(:radius this)) [(:p shape)] [])
      :line (math/intersect-line-circle (:p1 shape)(:p2 shape)(:p-center this)(:radius this))
      :circle (math/intersect-circles (:p-center this)(:radius this)(:p-center shape)(:radius shape))
      :arc  (math/intersect-circle-arc (:p-center this)(:radius this)
                                       (:p-center shape)(:radius shape)(:p-start shape)(:p-end shape))
            []))


  (between? [this q p1 p2]
    (math/on-arc? q (:p-center this)(:radius this) p1 p2))

  (points [this]
    [])


  (name-prefix [this]
    "Cir")

  (sort-points [this pnt-list]
    (->> pnt-list
         (filter #(math/on-circle? % (:p-center this)(:radius this)))
         (dedupe)
         (sort #((comparator (fn[p q](math/right-from? (first pnt-list) q p))) %1 %2))))

  (cut [this points]
    (if (< (count points) 2)
      [ :delete this]
      [ :delete this
        :create (constructArc (:p-center this)(:radius this)(second points)(first points))]))

  (transform-points [this f]
    (assoc this :p-center (f (:p-center this)) :p-ref (f (:p-ref this)))))


(defn constructCircle [p-center radius]
  (construct (->Circle p-center radius)))



;; 'arc' basic shape
;;
(defrecord Arc [p-center radius p-start p-end] IShape
  (construct [this]
    (-> this
      (assoc :type :arc
             :visible 1
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
      (assoc :p-start (math/vec-rotate p-start p-center angle)
             :p-end (math/vec-rotate p-end p-center angle)
             :p-ref (math/vec-rotate (:p-ref this) p-center angle))))

  (rotate-ref [this p-r angle]
    (assoc this :p-center (math/vec-rotate (:p-center this) p-r  angle)
                :p-start (math/vec-rotate p-start p-r angle)
                :p-end (math/vec-rotate p-end p-r angle)
                :p-ref (math/vec-rotate (:p-ref this) p-r angle)))


  (scale [this factor]
    (assoc this :radius (* radius factor)
                :p-start (math/vec-scale p-center p-start factor)
                :p-end (math/vec-scale p-center p-end factor)))

  (scale-ref [this p-r factor]
    (let [pc (math/vec-scale p-r (:p-center this) factor)
          v (math/vec-sub pc (:p-center this))
          radius (* radius factor)]
      (assoc (scale this factor) :p-center pc
                                 :radius radius
                                 :p-ref (math/project-point-onto-circle (math/vec-add (:p-ref this) v) pc radius)
                                 :p-start (math/project-point-onto-circle (math/vec-add (:p-start this) v) pc radius)
                                 :p-end (math/project-point-onto-circle (math/vec-add (:p-end this) v) pc radius))))

  (intersect [this shape]
    (case (:type shape)
      :point (if (math/on-arc? (:p shape)(:p-center this)(:radius this)(:p-start this)(:p-end this)) [(:p shape)] [])
      :line (math/intersect-line-arc (:p1 shape)(:p2 shape)(:p-center this)(:radius this)(:p-start this)(:p-end this))
      :circle (math/intersect-circle-arc (:p-center shape)(:radius shape)(:p-center this)(:radius this)(:p-start this)(:p-end this))
      :arc  (math/intersect-arcs (:p-center this)(:radius this)(:p-start this)(:p-end this)
                                 (:p-center shape)(:radius shape)(:p-start shape)(:p-end shape))
            []))


  (between? [this q p1 p2]
    (and
      (math/on-arc? q (:p-center this)(:radius this) p1 p2)
      (math/on-arc? p1 (:p-center this)(:radius this)(:p-start this)(:p-end this))
      (math/on-arc? p2 (:p-center this)(:radius this)(:p-start this)(:p-end this))))

  (points [this]
    [(:p-start this)(:p-end this)])

  (name-prefix [this]
    "Arc")

  (sort-points [this pnt-list]
    (->> pnt-list
         (filter #(math/on-arc? % (:p-center this)(:radius this)(:p-start this)(:p-end this)))
         (dedupe)
         (sort #((comparator (fn[p q](math/right-from? (:p-center this) q p))) %1 %2))))

  (cut [this [p q]]
    (if (math/equals? (:p-start this) p)
      (if (math/equals? (:p-end this) q)
        [ :delete this ]
        [ :delete this
          :create (assoc this :p-start q :p-ref q :name (:name this))])
      (if (math/equals? (:p-end this) q)
        [ :delete this :create (assoc this :p-end p :name (:name this))]
        [ :delete this :create (assoc this :p-end p)
          :create (constructArc (:p-center this)(:radius this) q (:p-end this))])))

  (transform-points [this f]
    (assoc this :p-center (f (:p-center this)) :p-start (f (:p-start this)) :p-end (f (:p-end this)) :p-ref (f (:p-ref this)))))

(defn constructArc [p-center radius p-start p-end]
  (construct (->Arc p-center radius p-start p-end)))



;; 'contour' basic shape
;;
(defn- linear-scale[factor coll]
  (map #(math/vec-scal-mult
         factor
         (math/vec-sub %1 %2)) (rest coll) coll))

(defrecord Contour [p-list] IShape
  (construct [this]
    (-> this
      (assoc :type :contour
             :visible 1
             :p-ref (first p-list))))

  (next-point [this p]
    (first
      (sort #(compare (last %1)(last %2))
        (map #(vec (list this % (math/dist p %))) (:p-list this)))))

  (translate [this v]
    (let [new-p-list (vec (map (partial math/vec-add v) p-list))]
      (assoc this :p-list new-p-list
                  :p-ref (first new-p-list))))

  (rotate [this angle]
    (rotate-ref this (:p-ref this) angle))

  (rotate-ref [this p-r angle]
    (assoc this :p-list (vec (map #(math/vec-rotate % p-r angle) p-list))
                :p-ref (math/vec-rotate (:p-ref this) p-r angle)))

  (scale [this factor]
    (let [[c1 c2] (split-with (partial math/not-equals? (:p-ref this)) (:p-list this))
           cls-l (linear-scale factor (cons (:p-ref this) (reverse c1)))
           cls-r (linear-scale factor c2)]
      (assoc this :p-list
                  (concat
                    (reverse (rest (reductions math/vec-add (:p-ref this) cls-l)))
                    [(:p-ref this)]
                    (rest (reductions math/vec-add (:p-ref this) cls-r))))))

  (scale-ref [this p-r factor]
    (assoc this :p-list (map #(math/vec-scale p-r % factor) (:p-list this))))

  (intersect [this shape]
;;NYI: ToBeDone
    [])


  (between? [this q p1 p2]
;;NYI: ToBeDone
    false)

  (points [this]
    (:p-list this))

  (name-prefix [this]
    "Con")

  (sort-points [this pnt-list]
;;NYI: ToBeDone
    [])

  (cut [this [p q]]
;;NYI: ToBeDone
    [])

  (transform-points [this f]
    (let [new-p-list (vec (map f p-list))]
      (assoc this :p-list new-p-list
                  :p-ref (first new-p-list)))))

(defn constructContour [p-list]
  (construct (->Contour (vec p-list))))


;; in addition to the rotate and scale operations with other IShapes,
;; an additional kind of rotation and scaling is appropriate for compounds:
;; rotate-all and scale-all perform the respective operation on every shape
;; the compound consists of. Only accessible for compounds!
(defn rotate-all [compound p-r angle]
  (if-not
    (and (satisfies? IShape compound)
         (= (:type compound) :compound))
    compound
    (let [e-list (map #(rotate-ref % p-r angle) (:elems compound))]
      (assoc compound :elems e-list
                      :p-ref (:p-ref (first e-list))))))

(defn scale-all [compound p-r factor]
  (if-not
    (and (satisfies? IShape compound)
         (= (:type compound) :compound))
    compound
    (let [e-list (map #(scale-ref % p-r factor) (:elems compound))]
      (assoc compound :elems e-list
                      :p-ref (:p-ref (first e-list))))))


;; 'compounds'
;;
(defrecord Compound [elems] IShape
  (construct [this]
    (-> this
      (assoc :type :compound
             :subtype :none
             :visible 0
             :p-ref (:p-ref (first elems)))))

  (next-point [this p]
    (first
      (sort #(compare (last %1)(last %2))
        (map #(next-point % p) (:elems this)))))

  (translate [this v]
    (let [e-list (map #(translate % v) (:elems this))]
      (assoc this :elems e-list
                  :p-ref (math/vec-add (:p-ref this) v))))

  (rotate [this angle]
    (rotate-ref this (:p-ref this) angle))

  (rotate-ref [this p-r angle]
    (assoc this :elems (map #(rotate-ref % p-r angle) (:elems this))
                :p-ref (math/vec-rotate (:p-ref this) p-r angle)))

  (scale [this factor]
    (scale-ref this (:p-ref this) factor))

  (scale-ref [this p-r factor]
    (assoc this :elems (map #(scale-ref % p-r factor) (:elems this))
                :p-ref (math/vec-scale p-r (:p-ref this) factor)))

  (intersect [this shape]
    (if-let [elems (:elems shape)]
      (let [cl1 (take-while (comp not empty? second) (iterate (fn [[a c]](list (first c)(rest c))) [(first elems)(rest elems)]))
            cl2 (map (fn[[a c]] (map (partial intersect a) c)) cl1)
            cl3 (map vec (reduce concat (reduce concat cl2)))]
        (dedupe (sort cl3)))))


  (between? [this q p1 p2]
;;NYI: ToBeDone
    false)

  (points [this]
    (dedupe
      (sort
        (map vec (apply concat (map points (:elems this)))))))

  (name-prefix [this]
    "Cmpnd")

  (sort-points [this pnt-list]
    [])

  (cut [this [p q]]
;;NYI: ToBeDone ???
    [])

  (transform-points [this f]
    (assoc this :elems (vec (map #(transform-points % f) (:elems this))) :p-ref (f (:p-ref this)))))


(defn constructCompound
  ([elems]
    (construct (->Compound elems)))
  ([elems key val & kvs]
    (last
      (map #(assoc (assoc (constructCompound elems) key val) (first %)(second %)) (partition 2 (concat (list key val) kvs))))))

;; 'text'
;;
(defrecord Text [str top-left bottom-right] IShape
  (construct [this]
    (-> this
      (assoc :type :text
             :visible 1
             :p-ref (math/vec-scale top-left bottom-right 0.5))))

  (next-point [this p]
    [this (:p this) (math/dist p (:p-ref this))])

  (translate [this v]
    (assoc this :top-left (math/vec-add (:top-left this) v)
                :bottom-right (math/vec-add (:bottom-right this) v)
                :p-ref (math/vec-add (:p-ref this) v)))

  (rotate [this angle]
    (rotate-ref this (:p-ref this) angle))

  (rotate-ref [this p-r angle]
    (assoc this :top-left (math/vec-rotate (:top-left this) p-r angle)
                :bottom-right (math/vec-rotate (:bottom-right this) p-r angle)
                :p-ref p-r))

  (scale [this factor]
    (scale-ref this (:p-ref this) factor))

  (scale-ref [this p-r factor]
    (assoc this :top-left (math/vec-scale p-r (:top-left this) factor)
                :bottom-right (math/vec-scale p-r (:bottom-right this) factor)
                :p-ref (math/vec-scale p-r (:p-ref this) factor)))

  (intersect [this shape]
    [])

  (between? [this q p1 p2]
    false)

  (points [this]
    [])


  (name-prefix [this]
    nil)

  (sort-points [this pnt-list]
    [])

  (cut [this [p q]]
    [])

  (transform-points [this f]
    (assoc this :top-left (f (:top-left this)) :bottom-right (f (:bottom-right this)) :p-ref (f (:p-ref this)))))


(defn constructText [str top-left bottom-right]
  (construct (->Text str top-left bottom-right)))
