(ns georepl.draw-framework
  (:require [clojure.core.match :refer [match]]
            [georepl.mathlib :as math]
            [georepl.draw-primitives :as dp]
            [quil.core :as quil]
            [quil.middleware :as m]
            [georepl.gui-base :as gui-base]
;            [georepl.gui :as gui]
            [georepl.gallery :as gallery]))


;;
;; gui framework functions
;;
(defn- setup-gui []
  (quil/background 255)
  (quil/text-size 10)
  (quil/no-fill)

;; reduced frame rate for testing purposes
;(quil/frame-rate 5)

  ; initial state
  (gui-base/init))


(defn- draw-gui [state]
  (quil/background 255)
  (gui-base/draw state))

(defn- key-pressed-gui [state key]
;(prn "KeyCode:" (:key-code key))
  (case (:key-code key)
    10  (gui-base/key-pressed state :ok)
    27  (gui-base/key-pressed state :esc)
    82  (gui-base/key-pressed state :redo)
    83  (gui-base/key-pressed state :save)
    90  (gui-base/key-pressed state :undo)
        (gui-base/key-pressed state (:key key))))


;;
;; main gui
;;
(defn init-frame-gui []
  (quil/defsketch GeoRepl
    :size [800 800]
    :features [:resizable]
    :title "GeoRepl"
    :setup setup-gui
    :draw draw-gui
    :update gui-base/update-frame
    :mouse-pressed gui-base/mouse-pressed
    :mouse-released gui-base/mouse-released
    :mouse-dragged gui-base/mouse-dragged
    :mouse-moved gui-base/mouse-moved
    :key-pressed key-pressed-gui
;    :on-close (gui/wrap gui/on-close)
    :middleware [m/fun-mode])
  nil)


;;
;; gallery framework functions
;;
(defn- setup-gallery [size drw-list f-on-close]
  (quil/background 255)
  (quil/no-fill)

;; reduced frame rate for testing purposes
(quil/frame-rate 5)

  ; initial state
  (gallery/init size drw-list f-on-close))


(defn- draw-gallery [state]
  (quil/background 255)
  (gallery/draw state))

(defn- update-frame [state]
  (if (true? (:complete state))
    (quil/exit)
    state))

(defn- key-pressed-gallery [state key]
  (case (:key-code key)
    10  (gallery/key-pressed state :ok)
    27  (gallery/key-pressed state :esc)
    82  (gallery/key-pressed state :redo)
    83  (gallery/key-pressed state :save)
    90  (gallery/key-pressed state :undo)
        (gallery/key-pressed state (:key key))))

;;
;; main gallery
;;
(defn init-frame-gallery [drw-list f-on-close]
  (quil/defsketch Gallery
    :size [600 600]
    :title "GeoRepl Gallery - Select A Drawing"
    :setup (fn [](setup-gallery [600 600] drw-list f-on-close))
    :draw draw-gallery
    :update update-frame
    :mouse-released gallery/mouse-released
    :mouse-moved gallery/mouse-moved
    :key-pressed key-pressed-gallery
    :middleware [m/fun-mode])
  nil)


