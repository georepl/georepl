(ns georepl.elements
  (:require [georepl.mathlib :as math])
  (:require [georepl.persistence :as pers]))

(def elems (atom '()))


;; regular elements have visibility > 0
;; others like compound elements have no visibility of their own since they are visible through their constituents and have visibility < 0
(defn list-elems []
  (filter #(> (:visible %) 0) @elems))


(defn- clear-temporary []
  (swap! elems filter #(pos? (:visible %)) @elems))


(defn- clear []
  (swap! elems empty))


(defn- elems-length []
  (count @elems))


(defn- tos []
  (first @elems))


(defn push-elem [e]
  (swap! elems conj e))


(defn pop-elem []
  (let [e (tos)]
    (swap! elems rest)
      e))


(defn- new-elem [e]
  (pop-elem)
  (push-elem e))


(defn- ppop-elem []
  (pop-elem)
  (pop-elem))


(defn write-elem
  ([name]
    (pers/write-drawing name @elems))
  ([name elem]
    (pers/write-drawing name [elem])))


(defn- slurp-elem [name]
  (doseq [elem (pers/read-drawing name)]
    (push-elem elem)))
