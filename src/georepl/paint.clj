(ns georepl.paint
  (:require [clojure.core.match :refer [match]]
            [georepl.mathlib :as math]
            [georepl.draw-primitives :as dp]
            [georepl.elements :as elements]
            [georepl.freehand :as freehand]
            [georepl.shapes :as shapes]
            [georepl.ask :as ask]
            [georepl.shapes-factory :as shapesFactory]))



;;
;;helper functions
;;
(defn snap-time-exceeded? [t]
  (> (- (System/currentTimeMillis) t)
    freehand/snap-duration))


(defn next-point-on-element [coll p]
  (if (empty? coll)
    nil
    (first
      (sort #(compare (last %1)(last %2))
        (map #(shapes/next-point % p) coll)))))


;;
;; state protocol interface
;;
(declare ->Asking ->Drawing)

(defprotocol IState
  (reset-state[this])
  (draw-temporary [this])
  (mouse-pressed[this event])
  (mouse-released[this event])
  (mouse-dragged[this event])
  (mouse-moved[this event])
  (key-pressed [this key])
  (picked [this])
  (snapped [this])
  (dashed [this])
  (dragged [this])
  (moved [this])
  (update-frame [this]))





;; contract for all protocol functions
(defn wrap [f]
  (fn wrap-inner-fn [& args]
    (when (nil? args)
      (throw (ex-info "IState-function called with no arguments" {:f f :state args})))

    (let [ret (apply f args)]
      (when-not (satisfies? IState ret)
        (throw (ex-info "non IState-function called or missing return value (state)" {:f f :state args :new-state ret})))
      ret)))



;;
;; the regular-drawing-mode implementation
;;
(defrecord Drawing [trace show-trace? complete?] IState
  (reset-state [this]
    (-> this
     (assoc :button-down-time nil
            :trace trace
            :show-trace? show-trace?
            :complete? complete?)))


  (draw-temporary [this]
    (when show-trace?
      (dp/draw-element {:params trace}))
    (assoc this :complete? false))



  (mouse-pressed[this event]
    (assoc this :trace (cons [(:x event)(:y event)(System/currentTimeMillis) 1] [])
                :button-down-time (System/currentTimeMillis)
                :show-trace? true
                :complete? false))


  (mouse-released[this event]
    (if (nil? (:button-down-time this))
      this
      (assoc this :trace (cons [(:x event)(:y event)(System/currentTimeMillis) 0] trace)
                  :button-down-time nil
                  :show-trace? false
                  :complete? true)))


  (mouse-dragged[this event]
    (assoc this :trace (cons [(:x event)(:y event)(System/currentTimeMillis) 1] trace)
                :show-trace? true))


  (mouse-moved [this event]
    this)

  (key-pressed [this key]
    this)


  (picked [this]
    this)


  (snapped [this]
    (if-let [[elem p d] (next-point-on-element
                          (elements/list-elems)
                          (first trace))]
      ((wrap reset-state)
        (->Asking p
                  (shapesFactory/createShapeFactory
                  (freehand/analyze-shape [p p]))))
      this))

  (dashed [this]
    (elements/pop-elem)
    ((wrap reset-state)
      (->Drawing '() false false)))

  (dragged [this]
    (let [elem (freehand/analyze-shape (map butlast trace))
          fact (shapesFactory/createShapeFactory elem)]
      (if (= (:type elem) :dashed)
        (dashed this)
        ((wrap reset-state)
          (->Asking (last trace)
                    fact)))))

  (moved [this]
    (if (> (count trace) 2)
      (assoc this :show-trace? true)
      this))


  (update-frame [this]
    "the ordinary drawing mode update-frame"
    (if-let [t (:button-down-time this)]
      (if (snap-time-exceeded? t)
        (snapped this)
        this)
      (let [trace trace]
        (match [(count trace) complete?]
               [0 _]     this
               [1 _]     (if (snap-time-exceeded? (freehand/timestamp (butlast (first trace))))
                           (snapped this)
                           (moved this))
               [2 false] (if (snap-time-exceeded? (freehand/timestamp (butlast (last trace))))
                           (snapped this)
                           (moved this))
               [_ true]  (dragged this)
               :else     (moved this))))))




;;
;; the answer-the-question mode implementation
;;
(defn modify [state]
  state)


(defrecord Asking [p-cur factory] IState
  (reset-state [this]
   (-> this
      (assoc :p-cur p-cur
             :f-cur (shapesFactory/current-question (:factory this))
;             :question (ask/ask p-cur (shapesFactory/current-question (:factory this)))
             :factory factory
             :button-down-time nil
             :back-to-drawing false
             :user-has? :done-nothing)))



  (draw-temporary [this]
    (when-let [e (shapesFactory/current-element factory)]
      (dp/draw-element e :orange)
      (dp/draw-point (:p-ref e) :orange))
    (when-let [sl (:question this)]
      (dp/draw-text-vec sl))
    this)


  (mouse-pressed [this event]
    (assoc this :p-cur [(:x event)(:y event)]
                :user-has? :dragged
                :button-down-time (System/currentTimeMillis)
                :trace [[(:x event)(:y event)(System/currentTimeMillis)]]))


  (mouse-released [this event]
    (if (nil? (:button-down-time this))
      this
      (assoc this :p-cur [(:x event)(:y event)]
                  :button-down-time nil
                  :user-has? (if (:user-has :dragged) :dragged :picked)
                  :trace [])))


  (mouse-dragged [this event]
    (let [elem (freehand/analyze-shape (:trace this))]
      (if (= (:type elem) :dashed)
        (assoc this :p-cur [(:x event)(:y event)]
                    :user-has? :dashed
                    :button-down-time nil
                    :trace (cons [(:x event)(:y event)(System/currentTimeMillis)] (:trace this)))
        (assoc this :p-cur [(:x event)(:y event)]
                    :user-has? :dragged
                    :trace (cons [(:x event)(:y event)(System/currentTimeMillis)] (:trace this))))))


  (mouse-moved [this event]
    (assoc this :p-cur [(:x event)(:y event)]
                :user-has? :moved))

  (key-pressed [this key]
    (let [ret (ask/key-pressed key (:question this))
          state (assoc this :question (first ret))]
      (if (nil? (second ret))
        state
        (let [quest (second ret)
              fact (:factory state)
              e (shapesFactory/current-element fact)]
          (case (:type quest)
            :immediate   (assoc state :factory ((:f quest) fact (:p-cur state)))
            :next-input  (let [p (:p-ref e)]
(println "State: " state)
                           (assoc state :f-cur (:f quest) :p-cur p))
                         state)))))


  (picked [this]
    (if (nil? (:f-cur this))
      this
      (if-let [fact ((:f-cur this) (:factory this) (:p-cur this))]
        (assoc this :factory fact :f-cur (shapesFactory/current-question fact)
                                  :user-has? :done-nothing)
        this)))



  (snapped [this]
    (if-let [[elem p d] (next-point-on-element (elements/list-elems) (:p-cur this))]
      (assoc this :p-cur p
                  :button-down-time nil
                  :back-to-drawing true)
      this))

  (dashed [this]
    ((wrap reset-state)
      (->Drawing '() false false)))

  (dragged [this]
    (if-not (nil? (:p-cur this))
      (assoc this :factory (shapesFactory/refresh
                             (:factory this)
                             (:p-cur this)))
      this))


  (moved [this]
    (if-not (nil? (:p-cur this))
      (assoc this :factory (shapesFactory/refresh
                             (:factory this)
                             (:p-cur this)))
      this))

  (update-frame [this]
    (if-let [t (:button-down-time this)]
      (if (snap-time-exceeded? t)
        (picked (snapped this))
        this)
      (let [fact (:factory this)
            e (shapesFactory/current-element fact)
            p (:p-cur this)
 ;;         f (:f-cur this)
            state (case (:user-has? this)
                         :picked   (picked this)
                         :dashed   (dashed this)
                         :dragged  (dragged this)
                         :moved    (moved this)
                                   this)]
          (if-not (:complete? (:factory state))
            state
            (let [shape (shapesFactory/finish (:factory state))]
              (if (and (= shape :line)    ;; in case of polygone mode: continue line drawing; consider points!
                       (not (:back-to-drawing state)))
                (let [elem (shapes/constructLine (:p-cur state)(:p-cur state))
                      fact (shapesFactory/createShapeFactory elem)]
                  ((wrap reset-state)
                    (->Asking (:p-cur state)
                              fact)))
                ((wrap reset-state)
                  (->Drawing '()
                             false
                             false)))))))))

;;
;;
(defn init[]
  ((wrap reset-state) (->Drawing '() false false)))


(defn draw
  [state]
    (doseq [e (elements/list-elems)]
      (dp/draw-element e))

    ((wrap draw-temporary) state))
