(ns georepl.elements
  (:require [georepl.mathlib :as math]
            [clojure.edn     :as edn]
            [clojure.java.io :as io]))

(def elems (atom '()))


;; regular elements have visibility > 0
;; others like compound elements have no visibility of their own since they are visible through their constituents and have visibility < 0
(defn list-elems []
  (filter #(> (:visible %) 0) @elems))


(defn clear-temporary []
  (swap! elems filter #(pos? (:visible %)) @elems))


(defn clear []
  (swap! elems empty))


(defn elems-length []
  (count @elems))


(defn tos []
  (first @elems))


(defn push-elem [e]
  (swap! elems conj e))


(defn pop-elem []
  (let [e (tos)]
    (swap! elems rest)
      e))


(defn new-elem [e]
  (pop-elem)
  (push-elem e))


(defn pop-elem []
  (let [e (tos)]
    (swap! elems rest)
      e))


(defn ppop-elem []
  (pop-elem)
  (pop-elem))


(defn write-file[name]
  (try
    (with-open [wrtr (io/writer name)]
      (loop [coll @elems]
        (if (empty? coll)
          nil
         (do
           (.write wrtr (prn-str (first coll)))
           (recur (rest coll))))))
    (catch java.io.FileNotFoundException e
      (throw e))
    (catch Exception e
      (throw e))))


(defn read-file[name]
  (try
    (with-open [rdr (io/reader name)]
      (doseq [line (line-seq rdr)]
        (push-elem (edn/read-string line))))
      (catch java.io.FileNotFoundException e
        (prn "Could not find file:" (:cause e))
        nil)
    (catch Exception e
      (throw e))))
