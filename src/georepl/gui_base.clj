(ns georepl.gui-base
  (:require [clojure.core.match :refer [match]]
            [georepl.draw-primitives :as dp]
            [georepl.gui :as gui]
            [georepl.mathlib :as math]
            [georepl.dialog :as dialog]
            [georepl.configuration :as config]))

;;
;; NYI: weird bug: There is a state attribute :context-menu which is only there to work around a bug which may
;; be in the quil framework or it's just another nasty synchronization problem. :context-menu should always have the
;; same value as :selection. Both values are set in the record constructors in gui.clj.
;; The value of :selection was changed in the constructor: The next time the status shows it comes with the former value
;; of :selection in the update-frame function (or whatever is called after the constructor).
;; Other attributes in the record keep their constructor-set values, however. Simply renaming :selection does not help.
;; So, as a workaround, :context-menu is initialized with the same value as :selection in the constructor and :selection is reset
;; with the value of :context-menu in reinit-state.
;;

;;
;; helpers
;;
(defn- trace-length [trace]
  (let [coll  (map (partial take 2) trace)]
    (reduce + (map math/dist (rest coll) coll))))


(defn- button-down-time-exceeded? [trace]
  (if (> (count trace) 2)
    (gui/snap-time-exceeded? (first (drop 2 (last trace)))(first (drop 2 (first trace))))
    (gui/snap-time-exceeded? (first (drop 2 (last trace))))))


(defn- set-context-mode [this p]
;(prn "guibase.set-context-mode:" (count (:selection this)))
 (let [sel (dialog/dialog p (:selection this))
       state (assoc this :selection sel :show-context? true)]
   state))


(defn key-pressed [this key]
  (case key
    :up     (let [sel (dialog/select :up (:selection this))]
              (if (empty? sel)
                this
                (assoc this :selection sel)))
    :down   (let [sel (dialog/select :down (:selection this))]
              (if (empty? sel)
                this
                (assoc this :selection sel)))
    :ok     (let [sel (dialog/select :ok (:selection this))]
              (if (empty? sel)
                this
                (assoc this :selection sel)))
    :save   (do
              (gui/save this)
              this)
    :undo   (if-let [e (gui/undo)]
              (assoc this :redo-stack
                          (cons e (:redo-stack this)))
              this)
    :redo   (if (empty? (:redo-stack this))
              this
              (do
                (gui/redo (first (:redo-stack this)))
                (assoc this :redo-stack (rest (:redo-stack this)))))
    :shift  (assoc this :raw-traces true)  ;NYI: ToDo: key deprecated???
            this))


(defn mouse-pressed [this event]
  (let [trace [[(:x event)(:y event)(System/currentTimeMillis)(:button event)]]
        p [(:x event)(:y event)]]
    (if (= :right (:button event))
      (set-context-mode this p)
      (if (:show-context? this)
        (if-let [sel (dialog/select (:x event)(:y event)(:selection this))]
          (assoc this
            :selection sel
            :context-menu sel
            :trace trace
            :f-context (:f (dialog/current-selection sel)))
          (assoc this :show-context? false))
        (assoc this :trace trace
                    :button-released -1)))))


(defn mouse-released [this event]
  (if (nil? (:button-released this))
    (assoc this :trace (cons [(:x event)(:y event)(System/currentTimeMillis) (last (first (:trace this)))] (:trace this))
                :button-released 1)
    (assoc this :trace (cons [(:x event)(:y event)(System/currentTimeMillis) (last (first (:trace this)))] (:trace this))
                :button-released (* -1 (:button-released this)))))


(defn mouse-dragged[this event]
  (assoc this :trace (cons [(:x event)(:y event)(System/currentTimeMillis)(:button event)] (:trace this))))


(defn mouse-moved [this event]
  (assoc this :trace (cons [(:x event)(:y event)(System/currentTimeMillis) nil] (:trace this))
              :mouse-moved? true
              :button-released -1))

(defn- reinit-state [this new-state]
  (let [mouse-moved? (if (nil? (:mouse-moved? this))
                       false
                       (:mouse-moved? this))]
    (assoc new-state :redo-stack (:redo-stack this)
                     :selection (:context-menu this)
                     :show-context? (:show-context? this)
                     :button-released (:button-released this)
                     :mouse-moved? mouse-moved?)))

(defn picked [this]
  (let [p (math/coordinates (first (:trace this)))
        state (gui/picked this p)]
    (reinit-state this (assoc state :trace [] :mouse-moved? false))))


(defn context [f this]
  (let [state (gui/context (dissoc this :f-context) f)]
    (assoc state :trace [] :show-context? false)))


(defn dashed [this]
  (let [state (gui/dashed this (:trace this))]
    (reinit-state this (assoc state :trace [] :mouse-moved? false))))

(defn snapped [this]
  (let [state (gui/snapped this (math/coordinates (first (:trace this))))]
    (reinit-state this (assoc state :trace [] :mouse-moved? false))))

(defn dragging [this]
  (gui/dragging this))

(defn dragged [this elem]
  (let [state (gui/dragged this (:trace this) elem)]
    (reinit-state this (assoc state :trace [] :mouse-moved? false))))

(defn moved [this]
  (let [state (gui/moved this (math/coordinates (first (:trace this))))]
    (reinit-state this (assoc state :trace (:trace this)))))


(defn update-frame [this]
  (gui/post-update-frame
    (if (:show-context? this)
      (if-let [f (:f-context this)]
        (context f (dissoc this :f-context))
        this)
      (if (= :left (last (first (:trace this))))
        (match [(gui/short-trace? (trace-length (:trace this)))
                (button-down-time-exceeded? (:trace this))]
          [true false]  (if (= 1 (:button-released this))
                          (picked this)
                          this)   ;still drawing ...
          [true  true]  (snapped (assoc this :button-released 0))
          [false _]     (match [(gui/dash-speed? (:trace this))(:button-released this)]
                           [true  1]  (dashed (assoc this :button-released 0))
                           [false 1]  (dragged (assoc this :button-released 0)
                                                  (dialog/current-selection (:selection this)))
                           [false -1] (dragging this)
                           :else      this   ;still drawing ...
                          ))
        (if (and (:mouse-moved? this) (not (empty? (:trace this))))
          (moved (assoc this :mouse-moved? false))
          (assoc this :mouse-moved? false))))))


(defn draw [this]
  (gui/draw this))

(defn init []
  (assoc (gui/init) :button-released 0
                    :mouse-moved? false))
