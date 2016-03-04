(ns georepl.elements
  (:require [georepl.mathlib :as math]
            [clojure.java.io :as io]))

(def elems (atom '()))


;; regular elements have visibility > 0
;; compound elements or templates have no visibility of their own since they;; elements in the making (on the workbench) have visibility < 0
(defn list-elems
  []
  (filter #(> (:visible %) 0) @elems))
;  (filter #(pos? (:visible %)) @elems))


;; TODO NYI: Deprecated???
(defn list-bench
  []
  (filter #(neg? (:visible %)) @elems))


(defn clear-temporary
  []
  (swap! elems filter #(pos? (:visible %)) @elems))


(defn clear
  []
  (swap! elems empty))


(defn elems-length
  []
  (count @elems))


(defn tos
  []
  (first @elems))


(defn push-elem
  [e]
  (swap! elems conj e))


(defn pop-elem
  []
  (let [e (tos)]
    (swap! elems rest)
      e))


(defn new-elem
  [e]
  (pop-elem)
  (push-elem e))


(defn pop-elem
  []
  (let [e (tos)]
    (swap! elems rest)
      e))


(defn ppop-elem
  []
  (pop-elem)
  (pop-elem))


(defn write-file[name]
  (with-open [wrtr (io/writer name)]
    (loop [coll @elems]
      (if (empty? coll)
        nil
       (do
         (.write wrtr (prn-str (first coll)))
         (recur (rest coll)))))))


(defn read-file[name]
  (with-open [rdr (io/reader name)]
    (doseq [line (line-seq rdr)]
      (println line)
;;      (push-elem line)
           )))
