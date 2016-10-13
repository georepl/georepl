(ns georepl.draw-primitives
  (:require [clojure.core.match :refer [match]]
            [georepl.mathlib :as math]
            [georepl.renderer :as renderer]))


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
  (renderer/exit))

;;
;; primitives
;;
(defn- draw-text
  ([s top-left bottom-right]
    (renderer/text s
                   (first top-left)
                   (second top-left)
                   (first bottom-right)
                   (second bottom-right)))
  ([s top-left bottom-right colour]
    (do
      (apply renderer/fill (colour colours))
      (draw-text s top-left bottom-right)
      (renderer/no-fill))))


(defn text-height [height]
  (renderer/text-size height))


(defn text-width [s]
  (renderer/text-width s))


;; NYI: use text-height instead of delta-y!!!
(defn- draw-name [name p]
  (let [v [(text-width name) delta-y]]
    (draw-text name p (math/vec-add p v) :black)))

(defn draw-str [s x1 y1 x2 y2]
  (apply renderer/fill (:black colours))
  (renderer/text s x1 y1 x2 y2)
  (renderer/no-fill))


(defn draw-point
  ([p]
    (if-not (and (coll? p)(= (count p) 2))
      (apply str (concat "argument " (str p) " must be two-dimensional vector"))
      (renderer/ellipse (first p) (second p) 4 4)))
  ([p colour]
    (if (coll? colour)
      (do
        (apply renderer/fill colour)
        (draw-point p)
        (renderer/no-fill))
      (draw-point p (:orange colours))))
  ([p colour size]
    (do
      (apply renderer/fill colour)
      (renderer/ellipse (first p) (second p) size size)
      (renderer/no-fill))))


(defn- draw-line
  ([p q]
    (renderer/line (first p)
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
      (renderer/arc (first p-center)
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
      (renderer/ellipse (first p-center)
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
          :line          (draw-line (:p1 elem)(:p2 elem)(:name elem))
          :arc           (draw-arc (:p-center elem)(:radius elem)(:p-start elem)(:p-end elem)(:name elem))
          :circle        (draw-circle (:p-center elem) (:radius elem)(:name elem))
          :text          (draw-text (:str elem) (:top-left elem) (:bottom-right elem) :black)
          :contour       (draw-contour (:p-list elem)(:name elem))
          :compound      (doseq [e (:elems elem)]
                           (draw-element e text-visible?))

                         (when-let [params (:params elem)]
                           (if (= (count params) 1)
                             (draw-point (first params))
                             (draw-contour params)))))))
  ([elem colour text-visible?]
    (when-let [colvec (colour colours)]
      (when (coll? colvec)
        (apply renderer/stroke colvec)))
    (draw-element elem text-visible?)
    (apply renderer/stroke (:black colours))))



(defn draw-text-vec[sel-coll]
  (let [e (first sel-coll)
        pnt-tl (:p1 e)
        pnt-br (:p2 e)]
    (when (not (or (nil? pnt-tl)(nil? pnt-br)))
      (renderer/text-size (- (second pnt-br)(second pnt-tl)))
      (doseq [e sel-coll]
        (apply renderer/fill
               (if (:highlight e)
                 (:orange colours)
                 (:someothercolour colours)))
        (draw-text (:s e) (:p1 e) (:p2 e)))
      (renderer/no-fill))))
