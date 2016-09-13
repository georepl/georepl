(ns georepl.draw-primitives
  (:require [clojure.core.match :refer [match]]
            [georepl.mathlib :as math]
            [quil.core :as quil]
            [quil.middleware :as m]))


(def colours {:orange [204 102 0]
              :red [250 0 0]
              :green [0 250 0]
              :blue [0 0 250]
              :black [0 0 0]
              :white [255 255 255]
              :someothercolour [50 0 0]})

(def delta-y 10)

;; cleanup old drawing before initializing new sketch
(defn exit[]
  (quil/exit))

;;
;; primitives
;;
(defn- draw-text
  ([s top-left bottom-right]
    (quil/text s (first top-left)
                 (second top-left)
                 (first bottom-right)
                 (second bottom-right)))
  ([s top-left bottom-right colour]
    (do
      (apply quil/fill (colour colours))
      (draw-text s top-left bottom-right)
      (quil/no-fill))))


(defn- draw-name [name p]
  (let [v [(quil/text-width name) delta-y]]
    (draw-text name p (math/vec-add p v) :black)))


(defn text-height [height]
  (quil/text-size height))


(defn text-width [s]
  (quil/text-width s))


(defn draw-str [s x1 y1 x2 y2]
  (apply quil/fill (:black colours))
  (quil/text s x1 y1 x2 y2)
  (quil/no-fill))


(defn draw-point
  ([p]
    (when-not (and (coll? p) (>= (count p) 2))
      (throw (ex-info "argument p must be two- or higher dimensional vector" {:p p :f draw-point})))
    (quil/ellipse (first p) (second p) 4 4))
  ([p colour]
    (if (coll? colour)
      (do
        (apply quil/fill colour)
        (draw-point p)
        (quil/no-fill))
      (draw-point p [204 102 0])))
  ([p colour size]
    (do
      (apply quil/fill colour)
      (quil/ellipse (first p) (second p) size size)
      (quil/no-fill))))


(defn- draw-line
  ([p q]
    (quil/line (first p)
               (second p)
               (first q)
               (second q)))
  ([p q name]
    (when (not (nil? name))
      (draw-name name (math/vec-add (math/vec-scale p q 0.5) [5.0 5.0])))
    (draw-line p q)))


(defn- draw-arc
  ([p-center radius p-start p-end]
    (let [angle-start (math/angle (math/vec-sub p-start p-center))
          angle-end (math/angle (math/vec-sub p-end p-center))
          diam (* radius 2)
          ang2 (if (< angle-start angle-end)
                 angle-end
                 (+ math/TWO-PI angle-end))]
      (quil/arc (first p-center)
                (second p-center)
                diam
                diam
                angle-start
                ang2)))
  ([p-center radius p-start p-end name]
    (when (not (nil? name))
      (let [br? (math/right-from? p-center p-end p-start)
            p-s (if br? p-start p-end)
            p-e (if br? p-end p-start)
            v (math/vec-sub p-s p-center)
            w (math/vec-sub p-e p-center)
            hp (math/vec-add p-center (math/vec-add v w))
            fct (/ (+ 10 radius)(max 1 (math/length (math/vec-sub hp p-center))))
            p (math/vec-scale p-center hp fct)]
        (draw-name name p)))
    (draw-arc p-center radius p-start p-end)))


(defn- draw-circle
  ([p-center radius]
    (let [diam (* radius 2)]
      (quil/ellipse (first p-center)
                    (second p-center)
                    diam
                    diam)))
  ([p-center radius name]
    (when (not (nil? name))
      (draw-name name (math/vec-add p-center [0 (+ radius 5)])))
    (draw-circle p-center radius)))


(defn- draw-contour
  ([points]
    (when (>= (count points) 2)
      (doseq [[p q] (map list points (rest points))]
        (draw-line p q))))
  ([points name]
    (when (not (nil? name))
      (draw-name name (first points)))
    (draw-contour points)))



(defn draw-element
  ([el text-visible?]
    (if (or (nil? el)(not= 1 (:visible el)))
      nil
      (let [elem (if text-visible? el (dissoc el :name))]
        (case (:type elem)
          :point         (do
                           (when-let [s (:name elem)]
                            (draw-name s (:p elem)))
                       (draw-point (:p elem)))
          :line          (draw-line (:p1 elem) (:p2 elem)(:name elem))
          :arc           (draw-arc (:p-center elem)(:radius elem)(:p-start elem)(:p-end elem)(:name elem))
          :circle        (draw-circle (:p-center elem) (:radius elem)(:name elem))
          :text          (draw-text (:str elem) (:top-left elem) (:bottom-right elem) :black)
          :contour       (draw-contour (:p-list elem)(:name elem))

                         (when-let [params (:params elem)]
                           (if (= (count params) 1)
                             (draw-point (first params))
                             (draw-contour params)))))))
  ([elem colour text-visible?]
    (when-let [colvec (colour colours)]
      (when (coll? colvec)
        (apply quil/stroke colvec)))
    (draw-element elem text-visible?)
    (apply quil/stroke (:black colours))))



(defn draw-text-vec[sel-coll]
  (let [e (first sel-coll)
        pnt-tl (:p1 e)
        pnt-br (:p2 e)]
    (when (not (or (nil? pnt-tl)(nil? pnt-br)))
      (quil/text-size (- (second pnt-br)(second pnt-tl)))
      (doseq [e sel-coll]
        (apply quil/fill
               (if (:highlight e)
                 (:orange colours)
                 (:someothercolour colours)))
        (draw-text (:s e) (:p1 e) (:p2 e)))
      (quil/no-fill))))
