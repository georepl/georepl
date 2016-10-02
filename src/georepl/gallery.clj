(ns georepl.gallery
  (:require [clojure.core.match :refer [match]]
            [georepl.mathlib :as math]
            [georepl.elements :as elements]
            [georepl.draw-primitives :as dp]))


;;
;;
(defn- in-box [x y frame]
  (let [box-p1 (nth frame 2)
        box-p2 (nth frame 3)]
  (if (and (< (first box-p1) x (first box-p2))
           (< (second box-p1) y (second box-p2)))
    frame
    nil)))


(defn- done [state]
  ((:f-on-close state) (second (:selected state)))
  (dp/exit)
  state)


(defn draw-temporary [state]
  (dp/draw-element {:type :contour :p-list (last (:selected state)) :visible 1} :orange false)
  state)


(defn mouse-released[state event]
  (if-let [fr (first (filter #(in-box (:x event)(:y event) %) (:frames state)))]
    (done (assoc state :selected fr :complete true))
    state))


(defn mouse-moved[state event]
  (if-let [fr (first (filter #(in-box (:x event)(:y event) %) (:frames state)))]
    (assoc state :selected fr)
    state))


(defn key-pressed [state key]
  (let [idx (first (:selected state))
        coll (:frames state)]
    (case key
      :ok     (done (assoc state :complete true))
      :up     (if (= idx 0)
                (assoc state :selected (nth coll (dec (count coll))))
                (assoc state :selected (nth coll (dec idx))))
      :left   (if (= idx 0)
                (assoc state :selected (nth coll (dec (count coll))))
                (assoc state :selected (nth coll (dec idx))))
      :down   (if (= idx (dec (count coll)))
                (assoc state :selected (nth coll 0))
                (assoc state :selected (nth coll (inc idx))))
      :right  (if (= idx (dec (count coll)))
                (assoc state :selected (nth coll 0))
                (assoc state :selected (nth coll (inc idx))))
              state)))


(defn- frame-list [elem]
  (if (and (= (:type elem) :compound)(= (:subtype elem) :frame))
    elem
    (if (and (= (:type elem) :compound)(= (:subtype elem) :drawing))
      (map frame-list (:elems elem))
      nil)))

(defn- recursive-extract-coll [coll]
  (let [elems (vec (reduce concat (map #(if (= (:type %) :compound) (:elems %) [%]) coll)))]
    (if (some #(= (:type %) :compound) elems)
      (recursive-extract-coll elems)
      elems)))

(defn- extract [frame]
  (let [elems (:elems frame)
        text (first elems)
        points (map :p1 (rest elems))
        x-list (map first points)
        y-list (map second points)]
    [(:str text)
     [(reduce min x-list)(reduce min y-list)]
     [(reduce max x-list) (reduce max y-list)]
     (vec (cons (last points) points))]))


(defn init [size drw-list f-on-close]
  (let [frames-raw (filter #(= (:type %) :compound)
                           (flatten
                             (map frame-list
                                  (:elems drw-list))))
        frames (map #(cons %2 %1) (map extract frames-raw) (range))]
   (assoc {} :elems (recursive-extract-coll [drw-list])
             :frames frames
             :selected (first frames)
             :complete false
             :f-on-close f-on-close)))


(defn draw [state]
  (doseq [e (:elems state)]
    (dp/draw-element e false))
  (draw-temporary state))
