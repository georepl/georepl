(ns georepl.elements
  (:require [georepl.mathlib :as math]
            [georepl.shapes :as shapes]
            [georepl.repl :as repl]
            [clojure.edn     :as edn]
            [clojure.java.io :as io]
            [georepl.configuration :as config]))


;; elements is a map which contains a stack of drawing compound elements. The top of stack drawing is the current one.
;; Walking down the stack rewinds the former states of the drawing (undo).
;; Every time a drawing is changed a new version of the drawing compound is pushed onto the stack.
;; The elements map contains a list of currently displayed shapes for performance reasons.
(def elements (atom '()))


;; reinitialize the whole elements stack
(defn clear []
  (swap! elements empty))


(defn- elements-length []
  (count @elements))


(defn- tos []
  (first @elements))


(defn- newest-shape []
  (first (:elems (:drw-elem (tos)))))


(defn- shapes-count []
  (count (:elems (:drw-elem (tos)))))


(defn- collect-shapes [elem]
  (if (= (:type elem) :compound)
    (map collect-shapes (:elems elem))
    elem))


(defn- push-drawing [elem]
  (assert
    (and (= (:type elem) :compound)(= (:subtype elem) :drawing))
    (prn-str "Element stack corrupt! {:bottom-element-on-stack" elem "}"))
  (let [shapes-list (filter
                      #(> (:visible %) 0)
                      (flatten (collect-shapes elem)))]
    (swap! elements  conj {:drw-elem elem :shapes-list shapes-list :watch-fn (:watch-fn (tos))})
    elem))


;; The drawings stack is empty iff no push has been performed yet.
;; The first pushed element must be a drawing which is guaranteed to survive as first element on the stack.
;; If a drawing is pushed this goes straight to the stack where it can be accessed using tos.
;; So, the stack represents all states of the current session's drawing.
(defn push-elem [e]
  (if (and (= (:type e) :compound)(= (:subtype e) :drawing))
    (push-drawing e)
    (let [drw (:drw-elem (tos))]
      (push-drawing (assoc drw :elems (cons e (:elems drw))))
      e)))


(defn pop-elem []
  (if (= 1 elements-length)
    nil
    (do
      (swap! elements rest)
      (tos))))


(defn list-elems []
  (:shapes-list (tos)))


(defn spit-drawing []
  (let [drw (:drw-elem (tos))]
    (when (or (not= (:type drw) :compound)(not= (:subtype drw) :drawing))
      (throw (ex-info "Element stack corrupt!" {:bottom-element-on-stack drw})))
    (if (empty? (:elems drw))
       nil
       (let [filename (apply str
                             (concat
                               (:drawings-directory config/Configuration)
                               (:filename drw)))]
         (spit filename (pr-str drw))))))


(defn slurp-drawing
  ([name]
    (push-elem (read-string (slurp name))))
  ([name f]
    (let [drw (read-string (slurp name))
          s (pr-str drw)]
      (push-elem (f drw)))))
