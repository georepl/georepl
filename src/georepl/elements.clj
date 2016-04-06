(ns georepl.elements
  (:require [georepl.mathlib :as math]
            [georepl.shapes :as shapes]
            [clojure.edn     :as edn]
            [clojure.java.io :as io]
            [georepl.configuration :as config]))


;; drawings-stack is a stack of drawing compound elements. The top pf stack drawing is the current one.
;; Walking down the stack rewinds the former states of the drawing (undo).
;; Every time a drawing is changed a new version of the drawing compound is pushed onto the stack.
(def drawings-stack (atom '()))


(defn clear []
  (swap! drawings-stack empty))


(defn- drawings-stack-length []
  (count @drawings-stack))


(defn- tos []
  (first @drawings-stack))

(defn- push-drawing [e]
  (swap! drawings-stack conj e))


;; The drawings stack is empty iff no push has been performed yet.
;; The first pushed element must be a drawing which is guaranteed to survive as first element on the stack.
;; If a drawing is pushed this goes straight to the stack (redo)
;; Other elements are added to a copy of tos before this is written to the stack
;; So, the stack is a stack of drawings representing all states of the current session
(defn push-elem [e]
  (if (empty? @drawings-stack)
    (if (and (= (:type e) :compound)(= (:subtype e) :drawing))
      (push-drawing e)
      (throw (ex-info "Element stack corrupt!" {:bottom-element-on-stack e})))
    (let [cur (tos)]
      (if (and (= (:type e) :compound)(= (:subtype e) :drawing))
        (push-drawing e)
        (push-drawing
          (assoc cur :elems (cons e (:elems cur))))))))


(defn pop-elem []
  (let [e (tos)]
    (if (= 1 drawings-stack-length)
      nil
      (do
        (swap! drawings-stack rest)
        e))))



;; regular elements have visibility > 0
;; others like compound elements have no visibility of their own since they are visible through their constituents and have visibility < 0
(defn- list-elems-part [elem]
  (if (= (:type elem) :compound)
    (map list-elems-part (:elems elem))
    elem))


(defn list-elems []
  (let [cur (tos)
        e-list (flatten (list-elems-part cur))]
    (filter #(> (:visible %) 0) e-list)))


(defn spit-drawing []
  (let [tos (tos)]
    (when (or (not= (:type tos) :compound)(not= (:subtype tos) :drawing))
      (throw (ex-info "Element stack corrupt!" {:bottom-element-on-stack tos})))
    (if (empty? (:elems tos))
       nil
       (let [filename (apply str
                             (concat
                               (:drawings-directory config/Configuration)
                               (:filename tos)))]
         (spit filename (pr-str tos))))))


(defn slurp-drawing
  ([name]
    (push-elem (read-string (slurp name))))
  ([name f]
    (let [drw (read-string (slurp name))
          s (pr-str drw)]
      (push-elem (f drw)))))
