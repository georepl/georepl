(ns georepl.renderer
  (:require [quil.core :as quil]
            [quil.middleware :as m]
            [georepl.elements :as elements]))


(defprotocol IRenderer
  (_setup [this size title setup update draw mouse-pressed mouse-released mouse-dragged mouse-moved key-pressed on-close])
  (_background [this rgb])
  (_frame-rate [this rate])
  (_exit[this])
  (_text [this s x1 y1 x2 y2][this s x y])
  (_fill [this r g b])
  (_no-fill [this])
  (_text-size [this height])
  (_text-width [this s])
  (_ellipse [this x y width height])
  (_line [this x1 y1 x2 y2])
  (_arc [this x y width height start stop])
  (_stroke[this rgb][this r g b]))


(defrecord Test [] IRenderer
  (_setup [this size title setup update draw mouse-pressed mouse-released mouse-dragged mouse-moved key-pressed on-close]
;    (prn "setup, SIZE: " size)
    (assoc this :f-renderer :setup))

  (_background [this rgb]
;    (prn "RENDERER: background" rgb)
    this)
  (_frame-rate [this rate]
;    (prn "RENDERER: frame-rate:" rate)
    this)
  (_exit [this]
    (assoc this :f-renderer :exit))
  (_text [this s x1 y1 x2 y2]
;    (prn "RENDERER: text" s x1 y1 x2 y2)
    this)
  (_text [this s x y]
;    (prn "RENDERER: text" s x y)
    this)
  (_fill [this r g b]
;    (prn "RENDERER: fill" r g b)
    (assoc this :f-renderer :fill))
  (_no-fill [this]
;    (prn "RENDERER: no-fill")
    (assoc this :f-renderer :no-fill))
  (_text-size [this height]
    1)
  (_text-width [this s]
    (count s))
  (_ellipse [this x y width height]
;    (prn "RENDERER: ellipse:" x y width height)
    (assoc this :f-renderer :ellipse))
  (_line [this x1 y1 x2 y2]
;    (prn "RENDERER: line:" [x1 y1] [x2 y2])
    this)
  (_arc [this x y width height start stop]
;    (prn "RENDERER: arc" x y width height start stop)
    this)
  (_stroke [this rgb]
,    (prn "RENDERER: stroke" rgb)
    this)
  (_stroke [this r g b]
;    (prn "RENDERER: stroke" r g b)
    this))


(defrecord Quil [] IRenderer
  (_setup [this size title setup update draw mouse-pressed mouse-released mouse-dragged mouse-moved key-pressed on-close]
    (quil/sketch
      :size size
      :features [:resizable]
      :title title
      :setup setup
      :draw draw
      :update update
      :mouse-pressed mouse-pressed
      :mouse-released mouse-released
      :mouse-dragged mouse-dragged
      :mouse-moved mouse-moved
      :key-pressed key-pressed
;    :on-close on-close
      :middleware [m/fun-mode]))

  (_background [this rgb]
    (quil/background rgb)
    this)

  (_frame-rate [this rate]
    (quil/frame-rate rate)
    this)

  (_exit[this]
    (quil/exit)
    this)

  (_text [this s x1 y1 x2 y2]
    (quil/text s x1 y1 x2 y2)
    this)
  (_text [this s x y]
    (quil/text s x y)
    this)

  (_fill [this r g b]
    (quil/fill r g b)
    this)

  (_no-fill [this]
    (quil/no-fill)
    this)

  (_text-size [this height]
    (quil/text-size height))

  (_text-width [this s]
    (quil/text-width s))

  (_ellipse [this x y width height]
    (quil/ellipse x y width height)
    this)

  (_line [this x1 y1 x2 y2]
    (quil/line x1 y1 x2 y2)
    this)

  (_arc [this x y width height start stop]
    (quil/arc x y width height start stop)
    this)

  (_stroke [this rgb]
    (quil/stroke rgb)
    this)
  (_stroke [this r g b]
    (quil/stroke r g b)
    this))


;; common interface
(defn init-renderer [key]
  (if (= key :quil)
    (elements/set-renderer (->Quil))
    (elements/set-renderer (->Test))))

(defn setup [size title setup update draw mouse-pressed mouse-released mouse-dragged mouse-moved key-pressed on-close]
  (_setup (elements/get-renderer) size title setup update draw mouse-pressed mouse-released mouse-dragged mouse-moved key-pressed on-close))

(defn background [rgb]
  (_background (elements/get-renderer) rgb))

(defn frame-rate [rate]
  (_frame-rate (elements/get-renderer) rate))

(defn exit[]
  (_exit (elements/get-renderer)))

(defn text
  ([s x1 y1 x2 y2]
    (_text (elements/get-renderer) s x1 y1 x2 y2))
  ([s x y]
    (_text (elements/get-renderer) s x y)))

(defn fill [r g b]
  (_fill (elements/get-renderer) r g b))

(defn no-fill []
  (_no-fill (elements/get-renderer)))

(defn text-size [height]
  (_text-size (elements/get-renderer) height))

(defn text-width [s]
  (_text-width (elements/get-renderer) s))

(defn ellipse [x y width height]
  (_ellipse (elements/get-renderer) x y width height))

(defn line [x1 y1 x2 y2]
  (_line (elements/get-renderer) x1 y1 x2 y2))

(defn arc [x y width height start stop]
  (_arc (elements/get-renderer) x y width height start stop))

(defn stroke
  ([rgb]
    (_stroke (elements/get-renderer) rgb))
  ([r g b]
    (_stroke (elements/get-renderer) r g b)))
