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
(def elements (atom []))

(defn- out[]
  (prn elements))


;; reinitialize the whole elements stack
(defn clear []
  (swap! elements empty))


(defn- elements-length []
  (count @elements))


(defn- tos []
  (last @elements))


(defn- newest-shape []
  (last (:elems (:drw-elem (tos)))))


; reads and removes the string representing the latest operation on an element.
(defn curform []
  (if-let [s (:to-repl (tos))]
    (do
      (swap! elements assoc-in [(dec (elements-length)) :to-repl] nil)
      s)
    nil))


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


(defn- push-drawing [drw elem]
  (assert
    (and (= (:type drw) :compound)(= (:subtype drw) :drawing))
    (prn-str "Element stack corrupt! :bottom-element-on-stack" drw))
  (let [shapes-list (filter
                      #(> (:visible %) 0)
                      (flatten (collect-shapes drw)))
        e-str (if (or (nil? elem)(nil? (:name elem)))
                nil
                (format "(def %s %s)" (:name elem) (pr-str elem)))
        new-drw {:drw-elem drw :shapes-list shapes-list :to-repl e-str}]
    (swap! elements  conj new-drw)
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
    (do
      (swap! elements (comp vec butlast))
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
