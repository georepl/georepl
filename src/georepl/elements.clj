(ns georepl.elements
  (:require [clojure.string  :as s]
            [georepl.mathlib :as math]
            [georepl.shapes  :as shapes]
            [clojure.edn     :as edn]
            [clojure.java.io :as io]
            [georepl.configuration :as config]))


;; elements is a map which contains a stack of drawing compound elements. The top of stack drawing is the current one.
;; Walking down the stack rewinds the former states of the drawing (undo).
;; Every time a drawing is changed a new version of the drawing compound is pushed onto the stack.
;; The elements map contains a list of currently displayed shapes for performance reasons.
(def elements (atom {:stack [] :upd-f nil}))

(defn- out[]
  (prn elements))


;; reinitialize the whole elements stack
(defn clear []
  (swap! elements assoc :stack []))


(defn- elements-length []
  (count (:stack @elements)))


(defn- tos []
  (last (:stack @elements)))


(defn- newest-shape []
  (last (:elems (:drw-elem (tos)))))


(defn- collect-shapes[elem]
  (if (= (:type elem) :compound)
    (map collect-shapes (:elems elem))
    elem))


(defn- collect-elements [elem]
  (if (= (:type elem) :compound)
    (cons elem (map collect-elements (:elems elem)))
    elem))


(defn- collect-named-elements [elem]
  (->> elem
       (collect-elements)
       (filterv #(not (nil? (:name %))))))


(defn- find-element-by-name [name]
  (->> (tos)
       (:drw-elem)
       (collect-elements)
       (filter #(= (:name %) name))
       (first)))


(defn- shapes-count []
  (count (collect-elements (:drw-elem (tos)))))


(defn register [f]
  (assert fn? f)
  (swap! elements assoc :upd-f f))


(defn- reinit-repl-server [elem]
  (if (or (nil? elem)(nil? (:name elem)))
    nil
    (if-let [f (:upd-f @elements)]
      (f (format "(def %s %s)" (:name elem) (pr-str elem)))
      nil)))


(defn- push-drawing [drw elem]
  (assert
    (and (= (:type drw) :compound)(= (:subtype drw) :drawing))
    (prn-str "Element stack corrupt! :bottom-element-on-stack" drw))
  (let [shapes-list (filter
                      #(> (:visible %) 0)
                      (flatten (collect-shapes drw)))
        points-list (dedupe (sort (concat (shapes/intersect drw drw)(shapes/points drw))))
        new-drw {:drw-elem drw :shapes-list shapes-list :points-list points-list}
        new-stack (conj (:stack @elements) new-drw)]
    (swap! elements assoc :stack new-stack)
    (reinit-repl-server elem)
    (if (nil? elem) drw elem)))


;; The drawings stack is empty iff no push has been performed yet.
;; The first pushed element is either a drawing loaded from a file or an empty one.
;; Every change of the current drawing's elements results in a new drawing which goes on top of this stack.
;; So the current drawing's elements can be accessed using tos.
(defn push-elem [e]
  (if (and (= (:type e) :compound)(= (:subtype e) :drawing))
    (push-drawing e nil)
    (let [drw (:drw-elem (tos))
          elems (conj (:elems drw) e)]
      (push-drawing (assoc drw :elems elems) e))))


(defn pop-elem []
  (if (= 1 elements-length)
    nil
    (let [new-stack (vec (butlast (:stack @elements)))]
      (swap! elements assoc :stack new-stack)
      (tos))))


(defn list-elems []
  (:shapes-list (tos)))


(defn list-points []
  (:points-list (tos)))


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


(defn- cut-name-str[prefix s]
  (if (s/starts-with? s prefix)
    (apply str (drop (count prefix) s))
    nil))


(defn- next-unused-index [idx-coll]
  (if (nil? idx-coll)
    1
    (inc (Integer/parseInt idx-coll))))


(defn unique-name [prefix]
  (->> (list-elems)
       (map :name)
       (map (partial cut-name-str prefix))
       (filter #(not (nil? %)))
       (sort #(compare (count %1)(count %2)))
       (last)
       (next-unused-index)
       (format "%s%d" prefix)))
