(ns georepl.gui
  (:require [clojure.core.match :refer [match]]
            [georepl.draw-primitives :as dp]
            [georepl.mathlib :as math]
            [georepl.elements :as elements]
            [georepl.freehand :as freehand]
            [georepl.shapes :as shapes]
            [georepl.shapes-factory :as shapesFactory]
            [georepl.configuration :as config]))

;;
;; to be moved to shapes-transformations.clj
;;
;; e-coll : all the shapes in the drawing
;; p-coll : all the points in the drawing
(defn- cut-element [e l p1 p-coll]
  (if (empty? p-coll)
    nil
    (let [pts1 (keep identity (map #(shapes/on-element % e) p-coll))  ;; consider the characteristic points on current shape
          pts2 (shapes/sort-points e pts1)
          p (first (sort #(compare (math/dist p1 %1)(math/dist p1 %2)) (shapes/intersect l e)))] ;; intersection dash line/element
      (first (keep (fn[[a b]](if (shapes/between? e p a b) [a b] nil))
                   (if (= (:type e) :circle)
                     (map list pts2 (rest (cycle pts2)))
                     (map list pts2 (rest pts2))))))))  ;; the part of e which intersects the dash line


(defn- cut-elements [p1 p2 e-coll p-coll]
  (let [l (shapes/constructLine p1 p2)     ;; linearized trace (dash line)
        elems (keep #(if (empty? (shapes/intersect l %)) nil %) e-coll)]   ;; those shapes which intersect the dash line
    (if (empty? p-coll)
      (reduce concat (map #(conj [] %1 %2) (repeat :delete) elems))
      (reduce concat (map #(shapes/cut % (cut-element % l p1 p-coll)) elems)))))

;;
;; pass local attributes from one mode to another
;;
(defn- attributes
  ([]
    ;; initialize
    {:show-pointlist? false
     :show-context? false
     :text-visible? true})
  ([state]
    ;; pass on attributes
    {:show-pointlist? (:show-pointlist? state)
     :show-context? (:show-context? state)
     :text-visible? (:text-visible? state)})
  ([state attrbs]
    ;; take attributes
    (merge state attrbs)))

;;
;; helper functions
;;
(defn snap-time-exceeded?
  ([t1 t2]
    (> (- t2 t1) (:snap-duration config/Configuration)))
  ([t]
    (snap-time-exceeded? t (System/currentTimeMillis))))

(defn short-trace? [s]
  (< s (:short-range config/Configuration)))

(defn dash-speed? [trace]
  (if (< (count trace) 5)
    false
    (let [v (vec (math/vec-sub (butlast (first trace))(butlast (last trace))))
          ds (math/length (butlast v))
          dt (last v)]
;(prn "DASH-SPEED:" ds "/" dt "=" (/ ds (+ dt math/EPS)) ">?>" (:dash-speed config/Configuration))
      (> (/ ds (+ dt math/EPS))(:dash-speed config/Configuration)))))

(defn- next-point-on-element [coll p]
  (if (empty? coll)
    nil
    (first
      (first
        (sort #(compare (last %1)(last %2))
          (map #(list % (math/dist % p)) coll))))))

(defn- next-element [p-list p]
  (if (empty? p-list)
    nil
    (first
      (sort #(compare (last %1)(last %2))
        (map #(shapes/next-point % p) p-list)))))

(defn get-shape [trace]
  (freehand/analyze-shape trace))

(defn- show-pointlist [state p]
  (assoc state :show-pointlist? (not (:show-pointlist? state))))

(defn- text-visible [state p]
  (assoc state :text-visible? (not (:text-visible? state))))


;;
;; state protocol interface
;;
(declare ->Drawing ->Creating ->Modifying)

(defprotocol IGui
  (reset-state[this])
  (draw-temporary [this])
  (picked [this p])
  (snapped [this p])
  (dashed [this trace])
  (dragging [this])
  (dragged [this trace elem])
  (moved [this p])
  (context [this f])
  (post-update-frame [this]))


;; contract for all protocol functions
(defn wrap [f]
  (fn wrap-inner-fn [& args]
    (when (nil? args)
      (throw (ex-info "IGui function called with no arguments" {:f f :state args})))

    (let [ret (apply f args)]
      (when-not (satisfies? IGui ret)
        (throw (ex-info "non-IGui function called or missing return value (state)" {:f f :state args :new-state ret})))
      ret)))

(defn- draw-polyline [state p]
  state)

(defn- draw-point [state p]
  ((wrap reset-state)
    (->Creating (:redo-stack state)
                (:selection state)
                (shapesFactory/createShapeFactory
                  (shapes/constructPoint p))
                (attributes state))))

(defn- modify-mode [state p]
;(prn "Modify")
  ((wrap reset-state)
    (->Modifying (:redo-stack state)
                 (:selection state)
                 (attributes state))))

(defn- drawing-dialog []
  (list {:s "Draw Shapes" :f draw-polyline :create :polyline :highlight true}
        {:s "Point" :f draw-point  :create :point :highlight false}
        {:s "Modify Shapes" :f modify-mode :create :modify :highlight false}
        {:s "Show Points" :f show-pointlist :highlight false}
        {:s "Show Names" :f text-visible :highlight true}))


(defn- create-elem [this trace e]
  (case (:create e)
    :point           (shapes/constructPoint (last trace))
                     (freehand/analyze-shape (map butlast trace))))


;;
;; the regular-drawing-mode implementation
;;
(defrecord Drawing [redo-stack selection attrbs] IGui
  (reset-state [this]
;(prn "->Drawing" (count selection))
    (-> this
        (attributes attrbs)
        (assoc :redo-stack redo-stack
               :selection selection
               :show-trace? false
               :context-menu selection)))

  (draw-temporary [this]
    (when (:show-trace? this)
      (dp/draw-element {:params (:trace this) :visible 1} :green false))
    this)

  (picked [this p]
(prn "Drawing picked!")
    (let [q (conj p nil)]
      (dragged this [q q] nil)))

  (snapped [this p]
    (if-let [q (next-point-on-element (elements/list-points) p)]
      ((wrap reset-state)
        (->Creating (:redo-stack this)
                    (:selection this)
                    (shapesFactory/createShapeFactory
                      (freehand/analyze-shape [q q]))
                    (attributes this)))
      this))

  ;; remove the previously drawn element and return to initial drawing mode
  (dashed [this trace]
    (let [todos (cut-elements (math/coordinates (first trace))(math/coordinates (last trace))(elements/list-elems)(elements/list-points))]
(prn "Drw. dashed, todos empty?:" (empty? todos))
      (if (empty? todos)
        (if-let [e (elements/pop-elem)]
          ((wrap reset-state)
             (->Drawing (cons e (:redo-stack this))
                        (:selection this)
                        (attributes this)))
          ((wrap reset-state)
             (->Drawing []
                        (:selection this)
                        (attributes this))))
        (do
          (elements/update-elements todos)
          this))))

  (dragging [this]
;(prn "Drawing Dragging:")
    (assoc this :show-trace? true))

  (dragged [this trace e]
;(prn "Drawing DRAGGED:")
    (let [state (assoc this :show-trace? false)
          elem (create-elem state trace e)]
      (if (= (:type elem) :dashed)
        (dashed state trace)
        ((wrap reset-state)
          (->Creating (:redo-stack state)
                      (:selection state)
                      (shapesFactory/createShapeFactory
                        elem)
                      (attributes state))))))

  (context [this f]
;(prn "Drawing context" (:selection this)(count (:trace this)))
(prn "Drawing context")
    (f this (math/coordinates (first (:trace this)))))

  (moved [this p]
;(prn "Drawing moved:" (:show-trace? this))
    (assoc this :trace[] :show-trace? false))

  (post-update-frame [this]
    this))


;;
;; the creation mode implementation
;;
(defn- finish-creation [state]
  (let [fact (:factory state)
        shape (shapesFactory/finish fact)]
    (if (or (:back-to-drawing? state)(:back-to-drawing? fact))
      ((wrap reset-state)
        (->Drawing [] (:selection-save state) (attributes state)))
      (case (first shape)
        :point  ((wrap reset-state)
                  (->Creating (:redo-stack state)
                              (:selection-save state)
                              (shapesFactory/createShapeFactory
                                (shapes/constructPoint (:p (second shape))))
                              (attributes state)))
        :line   ((wrap reset-state)
                  (->Creating []
                              (:selection-save state)
                              (shapesFactory/createShapeFactory
                                (assoc (shapes/constructLine (:p2 (second shape))(:p2 (second shape)))
                                               :ortho-polyline? (:ortho-polyline? (:elem (:factory state)))))
                              (attributes state)))

                (do
                  ((wrap reset-state)
                    (->Drawing []
                               (:selection-save state)
                               (attributes state))))))))


(defrecord Creating [redo-stack selection-save factory attrbs] IGui
  (reset-state [this]
;(prn "->Creating" (count selection-save)(count (shapesFactory/current-dialog factory)))
    (-> this
        (attributes attrbs)
        (assoc :redo-stack redo-stack
               :selection-save selection-save
               :factory factory
               :selection (shapesFactory/current-dialog factory)
               :back-to-drawing? false
               :context-menu (shapesFactory/current-dialog factory))))

  (draw-temporary [this]
;(prn "Creating.draw-temporary, ortho:" (:ortho-polyline? (shapesFactory/current-element (:factory this))))
    (when-let [e (shapesFactory/current-element (:factory this))]
      (dp/draw-element e :orange false)
      (dp/draw-point (:p-ref e) :blue))
    this)

  (picked [this p]
;(prn "Creating.picked :show-context? =" (:show-context? this))
    (if-let [fact (shapesFactory/current-question (:factory this) p)]
      (assoc this :factory fact)
      this))

  (snapped [this p]
;(prn "Creating snapped:" this)
    (if-let [q (next-point-on-element (elements/list-points) p)]
      (assoc (picked this q) :back-to-drawing? true)
      (picked this p)))

  (dashed [this trace]
(prn "Creating dashed:")
    ((wrap reset-state)
      (->Drawing (:redo-stack this)
                 (:selection-save this)
                 (attributes this))))

  (dragging [this]
    this)

  (dragged [this trace elem]
;(prn "Creating dragged:" (nil? this))
    (assoc this :factory (shapesFactory/refresh (:factory this) (first trace)))
    this)

  (context [this f]
;(prn "Creating context" f)
    (let [fact (f (:factory this) (math/coordinates (first (:trace this))))]
;(prn "Creating.context FACT:" fact)
      (assoc this :factory fact)))

  (moved [this p]
;(prn "Creating moved:" p (nil? this))
    (assoc this :factory (shapesFactory/refresh (:factory this) p)))

  (post-update-frame [this]
;(prn "SELECTION:" (count (:selection this)))
    (if (:complete? (:factory this))
      (finish-creation this)
      this)))


;
;; mofifications mode
;;
(defn- create-compound [this p]
(prn "create compound")
  this)

(defn- select-elements [state p]
(prn "select-elements")
  state)

(defn- drawing-mode [this p]
  ((wrap reset-state)
    (->Drawing nil
               (:selection-save this)
               (attributes this))))


(defn- modifying-dialog []
  (list {:s "Select elements" :f select-elements :highlight true}
        {:s "Create Compound" :f create-compound :highlight false}
        {:s "Drawing Mode" :f drawing-mode :highlight false}))

(defn- elem-in-out [this elem]
  (let [cur-elems (:current-elements this)
        new-elems  (vec (remove #(= % elem) cur-elems))]
    (if (= new-elems cur-elems)
      (cons elem cur-elems)
      new-elems)))

(defrecord Modifying [redo-stack selection-save attrbs] IGui
  (reset-state [this]
;(prn "->Modifying")
    (-> this
        (attributes attrbs)
        (assoc :redo-stack redo-stack
               :selection-save selection-save
               :selection (modifying-dialog)
               :context-menu (modifying-dialog)
               :current-elements []
               :p-list [])))

  (draw-temporary [this]
;(prn "Modifying.draw-temporary")
    (doseq [e (:current-elements this)]
      (dp/draw-element e :red false))
    (doseq [e (:p-list this)]
      (dp/draw-point e :green))
    this)

  ;; pick the nearest element and add it to elements list
  (picked [this p]
;(prn "Mod. picked" (:show-pointlist? this))
    (if-let [[elem p d] (next-element (elements/list-elems) p)]
      (assoc this :current-elements (elem-in-out this elem))
      this))

  ;; snap the nearest point (characteristic points of elements or intersections etc.)
  (snapped [this p]
;(prn "Mod. snapped")
      this)

  ;; Let l be the line between the first and the last trace point.
  ;; 1. if l intersects an existing shape: delete the part of the shape between the nearest points
  ;; 2. undo otherwise
  (dashed [this trace]
    (let [todos (cut-elements (math/coordinates (first trace))(math/coordinates (last trace))(elements/list-elems)(elements/list-points))]
(prn "Mod. dashed:" (count todos))
      (if (empty? todos)
        (if-let [e (elements/pop-elem)]
          ((wrap reset-state)
             (->Modifying (cons e (:redo-stack this))
                          (:selection this)
                          (attributes this)))
          ((wrap reset-state)
             (->Modifying []
                          (:selection this)
                          (attributes this))))
        (do
;(prn "process todos" )
          (elements/update-elements todos)
          this))))

  (dragging [this]
    this)

  (dragged [this trace elem]
;(prn "Mod. dragged" trace)
    this)

  (context [this f]
;(prn "Modifying context" (:selection this)(count (:trace this)))
    (let [p (take 2 (first (:trace this)))]
      (f this p)))

  (moved [this p]
    this)

  (post-update-frame [this]
    this))




;; framework functions outside of the above interface
;;
(defn init[]
;(prn "INIT")
  (dp/text-height (:shapes-label-size config/Configuration))
;  ((wrap reset-state) (->Modifying [] (modifying-dialog))))
  ((wrap reset-state) (->Drawing [] (drawing-dialog) (attributes))))

(defn draw [state]
  (doseq [e (elements/list-elems)]
    (dp/draw-element e (:text-visible? state)))
  (when (:show-context? state)
    (dp/text-height (:dialog-text-size config/Configuration))
    (dp/draw-text-vec (:selection state))
    (dp/text-height (:shapes-label-size config/Configuration)))
  (when (:show-pointlist? state)
    (doseq [p (elements/list-points)]
      (dp/draw-point p :blue)))
  ((wrap draw-temporary) state))

(defn undo []
  (elements/pop-elem))

(defn redo [elem]
  (elements/push-elem elem))

(defn save [this]
  (do
    (elements/spit-drawing)
    this))

(defn on-close [this]
  (save this))
