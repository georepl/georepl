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
              :white [255 255 255]})

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
      (apply quil/fill (:black colours))
      (draw-text s top-left bottom-right)
      (quil/no-fill))))

(defn text-height [height]
  (quil/text-size height))


(defn text-width [coll]
  (reduce max (map quil/text-width coll)))


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


(defn- draw-line [p q]
 (quil/line (first p)
            (second p)
            (first q)
            (second q)))


(defn- draw-arc [p-center radius angle-start angle-end]
  (let [diam (* radius 2)
        ang2 (if (< angle-start angle-end)
               angle-end
               (+ math/TWO-PI angle-end))]
    (quil/arc (first p-center)
              (second p-center)
              diam
              diam
              angle-start
              ang2)))


(defn- draw-circle [p-center radius]
  (let [diam (* radius 2)]
    (quil/ellipse (first p-center)
                  (second p-center)
                  diam
                  diam)))


(defn- draw-contour[points]
  (when (>= (count points) 2)
    (doseq [[p q] (map list points (rest points))]
      (draw-line p q))))


(defn draw-element
  ([elem]
    (if (nil? elem)
      nil
      (case (:type elem)
        :point   (draw-point (:p elem))
        :line    (when (= (:visible elem) 1)
                   (draw-line (:p1 elem) (:p2 elem)))
        :arc     (do
                  (draw-arc (:p-center elem)
                            (:radius elem)
                            (math/angle (math/vec-sub (:p-start elem)(:p-center elem)))
                            (math/angle (math/vec-sub (:p-end elem)(:p-center elem)))))
        :circle  (draw-circle (:p-center elem) (:radius elem))
        :text    (draw-text (:str elem) (:top-left elem) (:bottom-right elem) :black)
        :contour (when (= (:visible elem) 1)
                   (draw-contour (:p-list elem)))
                 (when-let [params (:params elem)]
                   (if (= (count params) 1)
                     (draw-point (first params))
                     (draw-contour params))))))
  ([elem colour]
    (when-let [colvec (colour colours)]
      (when (coll? colvec)
        (apply quil/stroke colvec)))
    (draw-element elem)
    (apply quil/stroke (:black colours))))



(defn draw-text-vec[ask-vec]
  (let [e (first ask-vec)
        pnt-tl (:p1 e)
        pnt-br (:p2 e)]
  (quil/text-size (- (second pnt-br)(second pnt-tl)))
  (doseq [e ask-vec]
    (do
      (apply quil/fill
             (if (pos? (:val e))
               [204 102 0]
               [50 0 0]))
      (draw-text (:s e) (:p1 e) (:p2 e))
  (quil/no-fill)))))
