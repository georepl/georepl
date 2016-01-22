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
  (moving [this])
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

  (moving [this]
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
                         (moving this))
             [2 false] (if (catch-time-exceeded? (freehand/timestamp (butlast (last trace)))
                                                 (freehand/timestamp (butlast (first trace))))
                         (caught this)
                         (moving this))
             [_ true]  (dragged this)
             :else     (moving this)))))




;;
;; the answer-the-question mode implementation
;;
(defn modify [state]
  state)



(defrecord Asking [p-cur factory] IState
  (reset-state [this]
   (-> this
      (assoc :p-cur p-cur
             :f-cur nil
             :question (ask/ask p-cur (shapesFactory/current-question (:factory this)))
             :factory factory
             :picked? false
             :dragged? false)))



  (update-frame [this]
    (let [fact (:factory this)
          e (shapesFactory/current-element fact)
          p (:p-cur this)
          f (:f-cur this)
          state (picked this)]
      (if-not (or (nil? f)(nil? p)(nil? fact))
        (assoc state :factory (f fact p)
                     :f-cur nil)
        (if-not (:complete? (:factory state))
          (dragged state)
          (do
            (shapesFactory/finish (:factory state))
            (reset-state (->Drawing '()
                                    false
                                    false)))))))



  (draw-temporary [this]
    (when-let [e (shapesFactory/current-element factory)]
      (dp/draw-element e)
      (dp/draw-point (:p-ref e) :orange))
    (when-let [sl (:question this)]
      (dp/draw-text-vec sl))
    this)


  (mouse-pressed [this event]
    this)


  (mouse-released [this event]
    (assoc this :p-cur [(:x event)(:y event)]
                :picked? true
                :dragged? false))


  (mouse-dragged [this event]
    (assoc this :p-cur [(:x event)(:y event)]
                :picked? false
                :dragged? true))


  (mouse-moved [this event]
    (assoc this :p-cur [(:x event)(:y event)]
                :picked? false
                :dragged? false))

  (key-pressed [this key]
    (let [ret (ask/key-pressed key (:question this))]
      (let [state (assoc this :question (first ret))]
        (if (nil? (second ret))
          state
          (let [e (second ret)
                f (:f e)]
(println "E:" e)
            (case (:type e)
              :immediate   (assoc state :factory ((:f e) (:factory state) (:p-cur state)))
              :next-input  (let [p (:p-ref (shapesFactory/current-element e))]
(println "P:" p)
                             (assoc state :f-cur (:f e) :p-cur p))
                           state))))))


  (picked [this]
    (if (and (:picked? this) (not (nil? (:f-cur this))))
      (assoc this :factory ((:f-cur this) (:factory this) (:p-cur this))
                  :f-cur nil)
      this))


  (caught [this]
    this)


  (dragged [this]
    (if-not (nil? (:p-cur this))
      (assoc this :factory (shapesFactory/refresh
                             (:factory this)
                             (:p-cur this)))
        this)))

;;
;;
(defn init[]
  ((wrap reset-state) (->Drawing '() false false)))


(defn draw
  [state]
    (doseq [e (elements/list-elems)]
      (dp/draw-element e))

    ((wrap draw-temporary) state))

