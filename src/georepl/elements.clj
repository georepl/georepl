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
;; Since this is the only atom within GeoRepl it contains other global information as well.
;; At program start a renderer for the graphical input/output is initialized and must remain available until the program is quit.
;; The renderer is kept in the value for the key :renderer.
;; The upd-f value is the place to store an optional update function. When a shape is changed in the Repl client the drawing won't change.
;; Of course not, since we don't change objects in Clojure, we create new objects. Whether this ('functional') approach will be maintained
;; or the drawing is held consistent with the Repl is a design decision still to be made. In the latter case we will need upd-f in the future.
;; The :selected-elem value holds the information which object the API functions in georepl.user rely to.
(def elements (atom {:stack [] :upd-f nil :selected-elem nil :renderer nil}))


(defn- out[]
  (prn elements))


;; reinitialize the whole elements stack
(defn clear []
  (swap! elements assoc :stack [] :upd-f nil :selected-elem nil))


(defn- elements-length []
  (count (:stack @elements)))


(defn- tos []
  (last (:stack @elements)))


(defn- collect-elements
  ([drw]
    (:elems drw))
  ([]
    (collect-elements (:drw-elem (tos)))))


(defn list-elements []
  (:elems (:drw-elem (tos))))

(defn- find-element-by-name [name]
  (if (nil? name)
    nil
    (->> (list-elements)
         (filter #(= (:name %) name))
         (first))))

(defn- newest-shape []
  (first (list-elements)))

(defn- collect-shapes[drw]
  (filter #(not= (:type %) :compound) (:elems drw)))


(defn expand-compounds [elems]
  (vec
    (dedupe
      (reduce concat
       (map #(if (= (:type %) :compound) (expand-compounds (:elems %)) (list %)) elems)))))


(defn- diff-set [coll subcoll]
  (if (empty? subcoll)
    coll
    (diff-set (filter (partial not= (first subcoll)) coll) (rest subcoll))))


(defn- colour-shapes [shapes sel-shapes]
  (let [sel (expand-compounds sel-shapes)]
    (vec
      (concat
        (map #(dissoc % :colour) (diff-set shapes sel))
        (map #(assoc % :colour :green) sel)))))


(defn select-elem [name]
  (let [selem (find-element-by-name name)
        shapes (vec (map #(dissoc % :colour) (:shapes-list (tos))))]
    (swap! elements assoc :selected-elem selem)
    (if (nil? selem)
      (swap! elements assoc-in [:stack (dec (elements-length)) :shapes-list] shapes)
      (swap! elements assoc-in [:stack (dec (elements-length)) :shapes-list] (colour-shapes shapes (vec (list selem)))))
    selem))


(defn cur-selected-elem []
  (:selected-elem @elements))


(defn register [f]
  (assert fn? f)
  (swap! elements assoc :upd-f f))

(defn set-renderer [renderer]
  (swap! elements assoc :renderer renderer))

(defn get-renderer []
  (:renderer @elements))

(defn- reinit-repl-server [elem]
  (if (or (nil? elem)(nil? (:name elem)))
    nil
    (if-let [f (:upd-f @elements)]
      (f (format "(def %s %s)" (:name elem) (pr-str elem)))
      nil)))


(defn- push-drawing [drw elem]
  (assert
    (and (= (:type drw) :compound)(= (:subtype drw) :drawing)(satisfies? shapes/IShape drw))
    (prn-str "Element stack corrupt! :bottom-element-on-stack" drw))
  (let [shapes-list (filter
                      #(> (:visible %) 0)
                      (flatten (collect-shapes drw)))
        points-list (dedupe (sort (concat (shapes/intersect drw drw)(shapes/points drw))))
        new-drw {:drw-elem drw :shapes-list shapes-list :points-list points-list}
        new-stack (conj (vec (:stack @elements)) new-drw)]
    (swap! elements assoc :stack new-stack)
    (reinit-repl-server elem)
    (if (nil? elem) drw elem)))


;; The drawings stack is empty iff no push has been performed yet.
;; The first pushed element is either a drawing loaded from a file or an empty one.
;; Every change of the current drawing's elements results in a new drawing which goes on top of this stack.
;; So, all the current drawing's elements can be accessed using tos.
(defn push-elem [e]
  (if (and (= (:type e) :compound)(= (:subtype e) :drawing))
    (push-drawing e nil)
    (let [drw (:drw-elem (tos))
          elems (cons e (vec (:elems drw)))]
      (push-drawing (assoc drw :elems elems) e))))


(defn push-elems [elems]
  (push-drawing (assoc (:drw-elem (tos)) :elems elems) nil))


(defn pop-elem []
  (if (= 1 elements-length)
    nil
    (let [new-stack (vec (butlast (:stack @elements)))]
      (swap! elements assoc :stack new-stack)
      (tos))))


(defn list-shapes []
  (:shapes-list (tos)))


(defn list-points []
  (:points-list (tos)))


;; unique names for elements
(defn- cut-name-str[prefix s]
  (if (and (string? s)(string? prefix)(s/starts-with? s prefix))
    (apply str (drop (count prefix) s))
    nil))


(defn- next-unused-index [idx-coll]
  (if (nil? idx-coll)
    1
    (inc (Integer/parseInt idx-coll))))


(defn unique-name
  ([prefix names-list]
    (let [name (->> (if (nil? names-list) [] names-list)
                    (sort)
                    (map (partial cut-name-str prefix))
                    (filter #(not (nil? %)))
                    (sort #(compare (count %1)(count %2)))
                    (last)
                    (next-unused-index)
                    (format "%s%d" prefix))]
       name))
  ([prefix]
    (unique-name prefix (map #(:name %) (list-shapes)))))



;; multiple push and pop operations in one transition
(defn update-elements
  ([todos] (update-elements todos (collect-shapes (:drw-elem (tos))) (map #(:name %) (list-shapes))))
  ([todos elems names-list]
    (if (empty? todos)
      (push-elems (vec (set elems)))
      (let [cmd (first todos)
            elem (second todos)
            remain-todos (vec (if (= (:type elem) :compound)
                                (concat (interleave (repeat cmd) (:elems elem))(nthrest todos 2))
                                (nthrest todos 2)))]
        (case cmd
          :create (let [nom (:name elem)
                        name (if (nil? nom)
                               (unique-name (shapes/name-prefix elem) names-list)
                               nom)]
                    (update-elements remain-todos (cons (assoc elem :name name) elems) (cons name names-list)))
          :delete (let []
                    (update-elements remain-todos (filter (partial not= elem) elems) names-list)))))))


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
