(ns georepl.ask
  (:require  [clojure.core.match :refer [match]]
             [georepl.draw-primitives :as dp]))




;; ask structure (context dialog)
;;
;;<string> <f> --> [<top-left> <bottom-right> <string> <f>]
(defn ask
  [[x y] ask-coll]
    (when-not (empty? ask-coll)
      (let [width (dp/text-width (map :s ask-coll))
            x-l x
            x-r (+ x-l width)
            height 20]
        (dp/text-height height)
        (map #(conj {:p1 (vec (list x-l %))}
                    [:p2 (vec (list x-r (+ % height)))]
                    [:s (:s %2)]
                    [:f (:f %2)]
                    [:type (:type %2)]
                    [:val (:val %2)])
             (iterate (partial + height) y) ask-coll))))


(defn up [coll]
  "scroll up the selection"
  (map #(assoc % :val (:val %2))
       coll
       (rest (cycle coll))))


(defn down [coll]
  "scroll down the selection"
  (reverse
    (up
      (reverse coll))))


(defn get-elem[coll]
  (first
    (drop-while #(not= (:val %) 1) coll)))


(defn key-pressed
  [key ask-coll]
    (case key
          :up     (let [coll (up ask-coll)]
                    [coll (get-elem coll)])
          :down   (let [coll (down ask-coll)]
                    [coll (get-elem coll)])
          :ok     [ask-coll (get-elem ask-coll)]
                  [ask-coll nil]))

