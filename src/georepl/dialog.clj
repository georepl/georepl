(ns georepl.dialog
  (:require  [clojure.core.match :refer [match]]
             [georepl.draw-primitives :as dp]
             [georepl.configuration :as config]))



;;<string> <f> --> [<top-left> <bottom-right> <string> <f>]
(defn dialog [[x y] sel-coll]
  (when-not (empty? sel-coll)
    (let [width (reduce max (map (comp dp/text-width :s) sel-coll))
          x-l x
          x-r (+ x-l width)
          height (:dialog-text-size config/Configuration)]
      (vec (map #(conj {:p1 (vec (list x-l %))}
                      [:p2 (vec (list x-r (+ % height)))]
                      [:s (:s %2)]
                      [:f (:f %2)]
                      [:create (:create %2)]
                      [:highlight (:highlight %2)])
           (iterate (partial + height) y) sel-coll)))))


;; context dialog
;;
(defn- in-box? [p q1 q2]
  (every? true? (map <= q1 p q2)))


(defn- select-point [p sel-coll]
  (if (nil? p)
    nil
    (if-let [e (first
                 (drop-while
                  #(not (in-box? p (:p1 %) (:p2 %)))
                  sel-coll))]
      e
      nil)))


(defn- up [coll]
  "scroll up the selection"
  (map #(assoc %1 :highlight (:highlight %2))
       coll
       (rest (cycle coll))))


(defn- down [coll]
  "scroll down the selection"
  (reverse
    (up
      (reverse coll))))


(defn current-selection [sel-coll]
  (first
    (drop-while #(= (:highlight %) 0) sel-coll)))


(defn select
  ([key sel-coll]
    (case key
          :up     (up sel-coll)
          :down   (down sel-coll)
          :ok     sel-coll
                  sel-coll))
  ([x y sel-coll]
    (if (nil? sel-coll)
      nil
      (if-let [e (select-point [x y] sel-coll)]
        (map #(assoc % :highlight (if (= e %) 1 0)) sel-coll)
        nil))))
