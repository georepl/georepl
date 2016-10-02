(ns georepl.user
  (:require [georepl.shapes :as shapes]
            [georepl.elements :as elements]
            [georepl.mathlib :as math]
            [clojure.tools.nrepl.server :as repl]
            [georepl.configuration :as config]))


;; load all shapes into the current namespace so they are conveniently accessible from a client
;;
(defn- update-elements [s]
  (if (seq? s)
    (doseq [s s]
      (load-string s))
    (load-string s)))


(defn start []
  [(repl/start-server
    :port (:repl-server-port config/Configuration))
   update-elements])


(defn stop [server]
  (repl/stop-server server))


;; 'public' interface for user interaction via repl
;;
(defn- out [s]
  (prn s))

;; contract for all protocol functions

(defn- decolour [elem]
  (if (= (:type elem) :compound)
    (assoc elem :elems (vec (map decolour (:elems elem))))
    (dissoc elem :colour)))

(defn select [name]
  (elements/select-elem name))

(defn- check-arguments [args coll-validation-fn]
  (if (not= (count args)(count coll-validation-fn))
    (do
      (out (format "%d arguments required" (count coll-validation-fn)))
      false)
    (every? (fn[[b s]](when (false? b)(out s)) b)
      (map #(list ((first %1) %2) (format (last %1) (str %2))) coll-validation-fn args))))


(defn point [p]
  (if (check-arguments [p] [[#(and (coll? %)(every? number? %)(= (count %) 2)) "<%s> is no real 2D vector!"]])
    (let [name (elements/unique-name "Pnt")
          elem (assoc (shapes/constructPoint p) :name name)]
      (elements/update-elements [:create elem])
      (select name)
      elem)
    nil))

(defn line [p q]
  (if (check-arguments [p q]
                       [[#(and (coll? %)(every? number? %)(= (count %) 2)) "<%s> is no real 2D vector!"]
                        [#(and (coll? %)(every? number? %)(= (count %) 2)) "<%s> is no real 2D vector!"]])
    (let [name (elements/unique-name "Ln")
          elem (assoc (shapes/constructLine p q) :name name)]
      (elements/update-elements [:create elem])
      (select name)
      elem)
    nil))

(defn circle
  ([cp radius]
    (if (check-arguments [cp radius]
                         [[#(and (coll? %)(every? number? %)(= (count %) 2)) "<%s> is no real 2D vector!"]
                          [#(and (number? %)(pos? %)) "<%s> is no positive number!"]])
      (let [name (elements/unique-name "Cir")
            elem (assoc (shapes/constructCircle cp radius) :name name)]
        (elements/update-elements [:create elem])
        (select name))
      nil))
  ([p1 p2 p3]
    (if (check-arguments [p1 p2 p3]
                         [[#(and (coll? %)(every? number? %)(= (count %) 2)) "<%s> is no real 2D vector!"]
                          [#(and (coll? %)(every? number? %)(= (count %) 2)) "<%s> is no real 2D vector!"]
                          [#(and (coll? %)(every? number? %)(= (count %) 2)) "<%s> is no real 2D vector!"]])
      (let [name (elements/unique-name "Cir")
            [center radius] (math/circumcircle p1 p2 p3)
            elem (assoc (shapes/constructCircle center radius) :name name)]
        (elements/update-elements [:create elem])
        (select name)
        elem)
      nil)))

(defn arc [cp radius start end]
  (if (check-arguments [cp radius]
                         [[#(and (coll? %)(every? number? %)(= (count %) 2)) "<%s> is no real 2D vector!"]
                          [#(and (number? %)(pos? %)) "<%s> is no positive number!"]])
    (let [name (elements/unique-name "Arc")
          elem (assoc (shapes/constructArc cp radius start end) :name name)]
      (elements/update-elements [:create elem])
      (select name)
      elem)
    nil))

(defn compound [& name]
  (let [coll1 (map identity name)]
    (if (every? string? coll1)
      (let [coll2 (map elements/select-elem coll1)]
        (if (not-any? nil? coll2)
          (let [name (elements/unique-name "Cmpnd")
                elem (assoc (shapes/constructCompound coll2) :name name)]
            (elements/update-elements [:create elem])
            (select name))
          (out "arguments must be names of elements")))
      (out "arguments must be strings (names of elements)"))))


(defn settings [& keyvals]
  (to-array keyvals))

(declare show)

(defn move [v]
  (if (check-arguments [v]
                       [[#(and (coll? %)(every? number? %)(= (count %) 2)) "<%s> is no real 2D vector!"]])
    (if-let [org-elem (elements/cur-selected-elem)]
      (let [name (:name org-elem)
            elem (assoc (shapes/translate org-elem v) :name name)]
        (elements/update-elements [:delete org-elem :create elem])
        (select name)
        elem)
      nil)
    nil))

(defn copy []
  (if-let [elem (elements/cur-selected-elem)]
    (let [name (name (elements/unique-name (shapes/name-prefix elem)))]
      (elements/update-elements [:create (assoc elem :name name)])
      (select name)
      elem)
    nil))

(defn rotate [angle]
  (if (check-arguments [angle] [[number? "<%s> is no real number!"]])
    (if-let [org-elem (elements/cur-selected-elem)]
      (let [name (:name org-elem)
            elem (shapes/rotate org-elem angle)]
        (elements/update-elements [:delete org-elem :create (assoc elem :name name)])
        (select name)
        elem)
      nil)
    nil))

(defn scale [factor]
  (if (check-arguments [factor] [[(and number? pos?) "<%s> is no positive real number!"]])
    (if-let [org-elem (elements/cur-selected-elem)]
      (let [name (:name org-elem)
            elem (shapes/scale org-elem factor)]
        (elements/update-elements [:delete org-elem :create (assoc elem :name name)])
        (select name)
        elem)
      nil)
    nil))

(defn mirror [p1 p2]
  (if (check-arguments [p1 p2]
                         [[#(and (coll? %)(every? number? %)(= (count %) 2)) "<%s> is no real 2D vector!"]
                          [#(and (coll? %)(every? number? %)(= (count %) 2)) "<%s> is no real 2D vector!"]])
    (if-let [org-elem (elements/cur-selected-elem)]
      (let [name (:name org-elem)
            elem (shapes/transform-points org-elem (partial math/mirror-point p1 p2))]
        (elements/update-elements [:delete org-elem :create (assoc elem :name name)])
        (select name)
        elem)
      nil)
    nil))

(defn undo []
  (elements/pop-elem))

(defn redo [elem]
  (elements/push-elem elem))

(defn show
  ([f-out mode]
    (case mode
      :list-elements   (f-out (elements/list-elements))
      :list-shapes     (f-out (elements/list-shapes))
      :elements-stack (f-out (elements/show-elements))
      :selected-elem  (f-out (elements/cur-selected-elem))
      (f-out "mode" mode "not implemented")))
  ([mode] (show out mode)))
