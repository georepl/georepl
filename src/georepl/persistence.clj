(ns georepl.persistence
  (:require [georepl.mathlib :as math]
            [clojure.edn     :as edn]
            [clojure.java.io :as io]))


(defn write-drawing [target coll]
(comment
  (try
    (with-open [wrtr (io/writer target)]
      (if (empty? coll)
        nil
        (do
          (.write wrtr (prn-str (first coll)))
          (recur (rest coll))))))
    (catch java.io.FileNotFoundException e
      (throw e))
    (catch Exception e
      (throw e)))
)

(defn read-drawing [source]
(comment
  (try
    (with-open [rdr (io/reader source)]
      (doseq [line (line-seq rdr)]
        (push-elem (edn/read-string line))))
      (catch java.io.FileNotFoundException e
        (prn "Could not find file:" (:cause e))
        nil)
    (catch Exception e
      (throw e))))


(defn write [elem]
  (pr elem))
)
