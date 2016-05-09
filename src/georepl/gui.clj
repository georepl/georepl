(ns georepl.gui
  (:require [clojure.core.match :refer [match]]
            [georepl.draw-primitives :as dp]
            [georepl.elements :as elements]
            [georepl.freehand :as freehand]
            [georepl.shapes :as shapes]
            [georepl.shapes-factory :as shapesFactory]
            [georepl.dialog :as dialog]
            [georepl.configuration :as config]))

;;
;;helper functions
;;
(defn- snap-time-exceeded? [t]
  (> (- (System/currentTimeMillis) t)
    freehand/snap-duration))

(defn- next-point-on-element [coll p]
  (if (empty? coll)
    nil
    (first
      (sort #(compare (last %1)(last %2))
        (map #(shapes/next-point % p) coll)))))


;;
;; state protocol interface
;;
(declare ->Drawing ->Creating)

(defprotocol IGui
  (reset-state[this])
  (draw-temporary [this])
  (mouse-pressed[this event])
  (mouse-released[this event])
  (mouse-dragged[this event])
  (mouse-moved[this event])
  (picked [this])
  (snapped [this])
  (dashed [this])
  (dragged [this])
  (moved [this])
  (context [this p])
  (update-frame [this]))


;; contract for all protocol functions
(defn wrap [f]
  (fn wrap-inner-fn [& args]
    (when (nil? args)
      (throw (ex-info "IGui function called with no arguments" {:f f :state args})))

    (let [ret (apply f args)]
      (when-not (satisfies? IGui ret)
        (throw (ex-info "non-IGui function called or missing return value (state)" {:f f :state args :new-state ret})))
      ret)))

(defn- draw-polyline [this]
  (prn "Polyline")
  this)

(defn- draw-ortho-polyline [this]
  (prn "Ortho-Polyline")
  this)

(defn- draw-point [this]
(prn "draw-point: Draw Point")
  ((wrap reset-state)
    (->Creating (:p-cur this)
                (shapesFactory/createShapeFactory
                  (shapes/constructPoint (:p-cur this)))
                (:redo-stack this)
                (:selection this))))

(defn- modify-mode [this]
  (prn "Modify")
  this)

(defn- cancel [state]
  (assoc state :selection nil))


(defn- drawing-dialog []
  (list {:s "Polyline" :f draw-polyline :create :polyline :highlight 1}
        {:s "Ortho-Polyline" :f draw-ortho-polyline :create :ortho-polyline :highlight 0}
        {:s "Point" :f draw-point  :create :point :highlight 0}
        {:s "Modify Shapes" :f modify-mode :create :modify :highlight 0}
        {:s "Cancel" :f cancel :highlight 0}))


(defn- create-elem [this]
  (let [e (dialog/current-selection (second (:selection this)))]
    (case (:create e)
      :point    (shapes/constructPoint (last (:trace this)))
                (freehand/analyze-shape (map butlast (:trace this))))))


;;
;; the regular-drawing-mode implementation
;;
(defrecord Drawing [redo-stack selection] IGui
  (reset-state [this]
    (-> this
     (assoc :redo-stack redo-stack
            :selection selection
            :current-dialog (if-let [sl (second selection)]
                              (first selection)
                              (drawing-dialog))
            :button-down-time nil
            :trace []
            :show-trace? false
            :complete? false)))

  (draw-temporary [this]
    (when (:show-trace? this)
      (dp/draw-element {:params (:trace this)}))
    (assoc this :complete? false))


  (mouse-pressed[this event]
    (if (= :right (:button event))
      (context this [(:x event)(:y event)])
      (if (last (:selection this))
        (let [[selection e visible?] (dialog/select (:x event)(:y event) (first (:selection this)))]
          (let [state (assoc this :selection [selection e visible?] :p-cur [(:x event)(:y event)])]
            ((:f e) state)
            state))
        (assoc this :trace (cons [(:x event)(:y event)(System/currentTimeMillis) 1] [])
                    :button-down-time (System/currentTimeMillis)
                    :show-trace? true
                    :complete? false))))


  (mouse-released[this event]
    (if (nil? (:button-down-time this))
      this
      (assoc this :trace (cons [(:x event)(:y event)(System/currentTimeMillis) 0] (:trace this))
                  :button-down-time nil
                  :show-trace? false
                  :complete? true)))

  (mouse-dragged[this event]
    (assoc this :trace (cons [(:x event)(:y event)(System/currentTimeMillis) 1] (:trace this))
                :show-trace? true))

  (mouse-moved [this event]
    this)

  (picked [this]
    this)

  (snapped [this]
    (if-let [[elem p d] (next-point-on-element
                          (elements/list-elems)
                          (first (:trace this)))]
      ((wrap reset-state)
        (->Creating p
                  (shapesFactory/createShapeFactory
                    (freehand/analyze-shape [p p]))
                  (:redo-stack this)
                  (:selection this)))
      this))

  ;; remove the previously drawn element and return to initial drawing mode
  (dashed [this]
    (if-let [e (elements/pop-elem)]
      ((wrap reset-state)
         (->Drawing (cons e (:redo-stack this)) (:selection this)))
      ((wrap reset-state)
         (->Drawing [] (:selection this)))))

  (dragged [this]
    (let [elem (create-elem this)
          fact (shapesFactory/createShapeFactory elem)]
      (if (= (:type elem) :dashed)
        (dashed this)
        ((wrap reset-state)
          (->Creating (last (:trace this))
                      fact
                      (:redo-stack this)
                      (:selection this))))))

  (moved [this]
    (if (> (count (:trace this)) 2)
      (assoc this :show-trace? true)
      this))

  (context [this p]
    (let [sel (dialog/dialog p (first (:selection this)))
          state (assoc this :selection [sel nil true])]
      state))

  (update-frame [this]
   "the ordinary drawing mode update-frame"
    (let [t (:button-down-time this)
          trace (:trace this)]
      (if (not (nil? t))
        (if (and (snap-time-exceeded? t)(<= (count trace) 3))
          (snapped this)
          this)
        (match [(count trace) (:complete? this)]
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
;; the creation mode implementation
;;
(defn- finish-creation [state]
  (let [shape (shapesFactory/finish (:factory state))]
    (match [shape (:back-to-drawing state)]
           [:point false] (let [elem (shapes/constructPoint (:p-cur state))
                                fact (shapesFactory/createShapeFactory elem)]
                           ((wrap reset-state)
                             (->Drawing [] (:selection-save state))))
           [:line false] (let [elem (shapes/constructLine (:p-cur state)(:p-cur state))
                               fact (shapesFactory/createShapeFactory elem)]
                           ((wrap reset-state)
                             (->Creating (:p-cur state) fact [] (:selection-save state))))

           :else         ((wrap reset-state)
                           (->Drawing [] (:selection-save state))))))



(defrecord Creating [p-cur factory redo-stack selection-save] IGui
  (reset-state [this]
   (-> this
      (assoc :redo-stack redo-stack
             :current-dialog (shapesFactory/current-dialog (:factory this))
             :selection-save selection-save
             :selection nil
             :p-cur p-cur
             :f-cur (shapesFactory/current-question (:factory this))
             :factory factory
             :button-down-time nil
             :back-to-drawing false
             :user-has? :done-nothing)))

  (draw-temporary [this]
    (when-let [e (shapesFactory/current-element factory)]
      (dp/draw-element e :orange)
      (dp/draw-point (:p-ref e) :orange))
    this)

  (mouse-pressed [this event]
    (let [p [(:x event)(:y event)]]
      (if (= :right (:button event))
        (context this p)
        (if-let [e (second (dialog/select (:x event)(:y event) (first (:selection this))))]
          ((:f e) this p)
          (assoc this :p-cur p
                      :user-has? :dragged
                      :button-down-time (System/currentTimeMillis)
                      :trace [[(:x event)(:y event)(System/currentTimeMillis)]])))))

  (mouse-released [this event]
    (if (nil? (:button-down-time this))
      this
      (assoc this :p-cur [(:x event)(:y event)]
                  :button-down-time nil
                  :user-has? (if (= (:user-has? this) :started-dashing)
                               :dashed
                               :picked)
                  :trace [])))

  ;; cancel drawing current shape
  (mouse-dragged [this event]
    (if (= (:user-has? this) :started-dashing)
      this
      (let [elem (freehand/analyze-shape (:trace this))]
        (if (= (:type elem) :dashed)
          (assoc this :p-cur [(:x event)(:y event)]
                      :user-has? :started-dashing
                      :button-down-time (System/currentTimeMillis)
                      :trace (cons [(:x event)(:y event)(System/currentTimeMillis)] (:trace this)))
          (assoc this :p-cur [(:x event)(:y event)]
                      :user-has? :dragged
                      :trace (cons [(:x event)(:y event)(System/currentTimeMillis)] (:trace this)))))))


  (mouse-moved [this event]
    (assoc this :p-cur [(:x event)(:y event)]
                :user-has? :moved))


  (picked [this]
;(prn "Picked:" this)
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
      (->Drawing (:redo-stack this) (:selection-save this))))

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

  (context [this p]
;    (assoc this :selection [(dialog/dialog p (:current-dialog this)) nil]))
    (let [sel (dialog/dialog p (:current-dialog this))
          state (assoc this :selection [sel nil true])]
;(prn "Creating Context:" state)
      state))

  (update-frame [this]
    (if-let [t (:button-down-time this)]
      (if (snap-time-exceeded? t)
        (picked (snapped this))
        this)
      (let [fact (:factory this)
            e (shapesFactory/current-element fact)
            p (:p-cur this)
            state (case (:user-has? this)
                         :picked   (picked this)
                         :dashed   (dashed this)
                         :dragged  (dragged this)
                         :moved    (moved this)
                                   this)]
         (if-not (:complete? (:factory state))
           state
           (finish-creation state))))))



;; framework functions outside of the above interface
;;
(defn key-pressed [this key]
;(prn "KEY-pressed" this)
  (case key
    :up     (if-let [sel (dialog/select :up (first (:selection this)))]
              (assoc this :selection sel)
              this)
    :down   (if-let [sel (dialog/select :down (first (:selection this)))]
              (assoc this :selection sel)
              this)
    :ok     (if-let [sel (dialog/select :ok (first (:selection this)))]
              (assoc this :selection sel)
              this)
    :save   (do
              (elements/spit-drawing)
              this)
    :undo   (if-let [e (elements/pop-elem)]
              (assoc this :redo-stack
                          (cons e (:redo-stack this)))
              this)
    :redo   (if (empty? (:redo-stack this))
              this
              (do
                (elements/push-elem (first (:redo-stack this)))
                (assoc this :redo-stack (rest (:redo-stack this)))))
    :shift  (assoc this :raw-traces true)
            this))


(defn on-close [this]
  (do
    (elements/spit-drawing)
    this))


(defn init[]
  (dp/text-height (:shapes-label-size config/Configuration))
  ((wrap reset-state) (->Drawing [] [(drawing-dialog) nil false])))


(defn draw [state]
  (doseq [e (elements/list-elems)]
    (dp/draw-element e))
  (when-let [sl (first (:selection state))]
    (when (last (:selection state))
;(when (nil? (:p1 sl)) (prn "SL:" sl))
      (dp/text-height (:dialog-text-size config/Configuration))
      (dp/draw-text-vec sl)
      (dp/text-height (:shapes-label-size config/Configuration))))
  ((wrap draw-temporary) state))
