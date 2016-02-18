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
(defn catch-time-exceeded? [t1 t2]
 (> (- t2 t1)
    freehand/catch-duration))


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
  (caught [this])
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
     (assoc :trace trace
            :show-trace? show-trace?
            :complete? complete?)))


  (draw-temporary [this]
    (when show-trace?
      (dp/draw-element {:params trace}))
    (assoc this :complete? false))



  (mouse-pressed[this event]
    (assoc this :trace (cons [(:x event)(:y event)(System/currentTimeMillis) 1] [])
                :show-trace? true
                :complete? false))


  (mouse-released[this event]
    (assoc this :trace (cons [(:x event)(:y event)(System/currentTimeMillis) 0] trace)
                :show-trace? false
                :complete? true))


  (mouse-dragged[this event]
    (assoc this :trace (cons [(:x event)(:y event)(System/currentTimeMillis) 1] trace)
                :show-trace? true))


  (mouse-moved [this event]
    this)

  (key-pressed [this key]
    this)


  (picked [this]
    (freehand/analyze-shape (butlast (first trace)) :point)
    (reset-state this))


  (caught [this]
    (let [p (first trace)]
      (if (= (last p) 1) ;ignore "mouse-released" after "caught"
        (freehand/analyze-shape (butlast p) :catchpoint))
    (reset-state this)))


  (dragged [this]
    (let [elem (freehand/analyze-shape (map butlast trace) :contour)
          fact (shapesFactory/createShapeFactory elem)]
      (case (:type elem)
        :dashed    (elements/pop-elem)
        :none      (println "Factory Kaputt:" fact)
                   (reset-state (->Asking (last trace)
                                          fact)))))

  (moved [this]
    (if (> (count trace) 2)
      (assoc this :show-trace? true)
      this))


  (update-frame [this]
    "the ordinary drawing mode update-frame"
    (let [trace trace]
      (match [(count trace) complete?]
             [0 _]     this
             [1 _]     (if (catch-time-exceeded? (freehand/timestamp (butlast (first trace)))
                                                 (System/currentTimeMillis))
                         (caught this)
                         (moved this))
             [2 false] (if (catch-time-exceeded? (freehand/timestamp (butlast (last trace)))
                                                 (freehand/timestamp (butlast (first trace))))
                         (caught this)
                         (moved this))
             [_ true]  (dragged this)
             :else     (moved this)))))




;;
;; the answer-the-question mode implementation
;;
(defn modify [state]
  state)

(defn next-point-on-element
  [coll p]
  (first
    (sort #(compare (last %1)(last %2))
      (map #(shapes/next-point % p) coll))))


(defrecord Asking [p-cur factory] IState
  (reset-state [this]
   (-> this
      (assoc :p-cur p-cur
             :f-cur (shapesFactory/initial-question (:factory this))
             :question (ask/ask p-cur (shapesFactory/current-question (:factory this)))
             :factory factory
             :user-has? :done-nothing)))



  (draw-temporary [this]
    (when-let [e (shapesFactory/current-element factory)]
      (dp/draw-element e :orange)
      (dp/draw-point (:p-ref e) :orange))
    (when-let [sl (:question this)]
      (dp/draw-text-vec sl))
    this)


  (mouse-pressed [this event]
(println "mouse-pressed")
    (assoc this :p-cur [(:x event)(:y event)]
                :user-has? :dragged
                :trace [[(:x event)(:y event)(System/currentTimeMillis)]]))


  (mouse-released [this event]
    (let [p (freehand/catch-point (cons [(:x event)(:y event)(System/currentTimeMillis)] (:trace this)))]
      (if (nil? p)
        (assoc this :p-cur [(:x event)(:y event)]
                    :user-has? (if (:user-has :dragged) :dragged :picked)
                    :trace [])
        (caught (assoc this :p-cur p
                            :user-has? :picked
                            :trace [])))))


  (mouse-dragged [this event]
(println "mouse-dragged")
;(println "mouse-dragged:" (:trace this))
    (assoc this :p-cur [(:x event)(:y event)]
                :user-has? :picked
                :trace (cons [(:x event)(:y event)(System/currentTimeMillis)] (:trace this))))


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
        (if (= fact (:factory this))
          this
          (assoc this :factory fact
                      :f-cur nil)))))



  (caught [this]
;(println "NPOE:" (next-point-on-element (elements/list-elems) (:p-cur this)))
    (if-let [[elem p d] (next-point-on-element (elements/list-elems) (:p-cur this))]
      (assoc-in
       (assoc this :p-cur p)
       [:factory :complete?] true)
      this))


  (dragged [this]
;(println "Dragged" (:f-cur this)(:p-cur this))
    (if-not (nil? (:p-cur this))
      (assoc this :factory (shapesFactory/refresh
                             (:factory this)
                             (:p-cur this)))
      this))


  (moved [this]
;(println "Moved")
    (if-not (nil? (:p-cur this))
      (assoc this :factory (shapesFactory/refresh
                             (:factory this)
                             (:p-cur this)))
      this))

  (update-frame [this]
    (let [fact (:factory this)
          e (shapesFactory/current-element fact)
          p (:p-cur this)
          f (:f-cur this)
          state (case (:user-has? this)
                       :picked   (picked this)
                       :dragged  (dragged this)
                       :moved    (moved this)
                                 this)]
        (if-not (:complete? (:factory state))
          state
          (let [shape (shapesFactory/finish (:factory state))]
            (if (= shape :line)    ;; in case of polygone mode: continue line drawing; consider points!
              (let [elem (shapes/constructLine (:p-cur this)(:p-cur this))
                    fact (shapesFactory/createShapeFactory elem)]
                 (reset-state (->Asking (:p-cur this)
                                          fact)))
              (reset-state (->Drawing '()
                                      false
                                      false))))))))

;;
;;
(defn init[]
  ((wrap reset-state) (->Drawing '() false false)))


(defn draw
  [state]
    (doseq [e (elements/list-elems)]
      (dp/draw-element e))

    ((wrap draw-temporary) state))

