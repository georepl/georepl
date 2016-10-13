(ns georepl.draw-framework
  (:require [clojure.core.match :refer [match]]
            [georepl.mathlib :as math]
            [georepl.draw-primitives :as dp]
            [georepl.renderer :as renderer]
            [georepl.gui-base :as gui-base]
            [georepl.gallery :as gallery]))


(defn init-renderer [key]
  (renderer/init-renderer key))

;;
;; gui framework functions
;;
(defn- setup-gui []
  (renderer/background 255)
  (renderer/text-size 10)
  (renderer/no-fill)

;; reduced frame rate for testing purposes
;(renderer/frame-rate 5)

  ; initial state
  (gui-base/init))


(defn- draw-gui [state]
  (renderer/background 255)
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
  (renderer/setup
              [800 800]
              "GeoRepl"
              setup-gui
              gui-base/update-frame
              draw-gui
              gui-base/mouse-pressed
              gui-base/mouse-released
              gui-base/mouse-dragged
              gui-base/mouse-moved
              key-pressed-gui
              nil    ;;(gui/wrap gui/on-close)
    ))


;;
;; gallery framework functions
;;
(defn- setup-gallery [size drw-list f-on-close]
  (renderer/background 255)
  (renderer/no-fill)

;; reduced frame rate for testing purposes
;(renderer/frame-rate 5)

  ; initial state
  (gallery/init size drw-list f-on-close))


(defn- draw-gallery [state]
  (renderer/background 255)
  (gallery/draw state))

(defn- update-frame [state]
  (if (true? (:complete state))
    (renderer/exit)
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
  (renderer/setup  [600 600]
                   "GeoRepl Gallery - Select A Drawing"
                   (fn [](setup-gallery [600 600] drw-list f-on-close))
                   update-frame
                   draw-gallery
                   nil
                   gallery/mouse-released
                   nil
                   gallery/mouse-moved
                   key-pressed-gallery
                   nil))
