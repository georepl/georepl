(ns georepl.elements
  (:require [georepl.mathlib :as math]
            [georepl.shapes :as shapes]
            [clojure.edn     :as edn]
            [clojure.java.io :as io]))


;; drawings-stack is a stack of drawing compound elements. The top pf stack drawing is the current one.
;; Walking down the stack rewinds the former states of the drawing (undo).
;; Every time a drawing is changed a new version of the drawing compound is pushed onto the stack.
(def drawings-stack (atom '()))


(defn- clear []
  (swap! drawings-stack empty))


(defn- drawings-stack-length []
  (count @drawings-stack))


(defn- tos []
  (first @drawings-stack))

(defn- push-drawing [e]
  (swap! drawings-stack conj e))


(defn push-elem [e]
  (if (and (empty? @drawings-stack)(= (:type e) :compound)(= (:subtype e) :drawing))
    (push-drawing e)
    (let [cur (tos)]
      (push-drawing
        (assoc cur :elems (cons e (:elems cur)))))))

(defn pop-elem []
  (let [e (tos)]
    (swap! drawings-stack rest)
      e))



;; regular elements have visibility > 0
;; others like compound elements have no visibility of their own since they are visible through their constituents and have visibility < 0
(defn list-elems []
  (let [cur (tos)]
    (filter #(> (:visible %) 0) (:elems cur))))





(defn spit-drawing []
  (if (not (or (= (:type (tos)) :compound) (= (:subtype (tos)) :drawing)))
    (prn "throw Invalid format")
    (if (empty? (:elems (tos)))
       nil
       (spit (:filename (tos)) (prn-str (tos))))))

(defn slurp-drawing [name]
  (push-elem (read-string (slurp name))))
