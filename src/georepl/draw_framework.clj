(ns georepl.draw-framework
  (:require [clojure.core.match :refer [match]]
            [georepl.mathlib :as math]
            [georepl.draw-primitives :as dp]
            [quil.core :as quil]
            [quil.middleware :as m]
            [georepl.paint :as paint]
            [georepl.gallery :as gallery]))


;;
;; paint framework functions
;;
(defn- setup-paint []
  (quil/background 255)
  (quil/no-fill)

;; reduced frame rate for testing purposes
;(quil/frame-rate 5)

  ; initial state
  (paint/init))


(defn- draw-paint [state]
  (quil/background 255)
  (paint/draw state))

(defn- key-pressed-paint [state key]
;(prn "KeyCode:" (:key-code key))
  (case (:key-code key)
    10  (paint/key-pressed state :ok)
    27  (paint/key-pressed state :esc)
    82  (paint/key-pressed state :redo)
    83  (paint/key-pressed state :save)
    90  (paint/key-pressed state :undo)
        (paint/key-pressed state (:key key))))


;;
;; main
;;
(defn init-frame-paint [size]
  (quil/defsketch GeoRepl
    :size [800 800]
    :features [:resizable]
    :title "GeoRepl"
    :setup setup-paint
    :draw draw-paint
    :update (paint/wrap paint/update-frame)
    :mouse-pressed (paint/wrap paint/mouse-pressed)
    :mouse-released (paint/wrap paint/mouse-released)
    :mouse-dragged (paint/wrap paint/mouse-dragged)
    :mouse-moved (paint/wrap paint/mouse-moved)
    :key-pressed key-pressed-paint
    :on-close (paint/wrap paint/on-close)
    :middleware [m/fun-mode])
  nil)





;;
;; gallery framework functions
;;
(defn- setup-gallery [size]
  (quil/background 255)
  (quil/no-fill)

;; reduced frame rate for testing purposes
;(quil/frame-rate 5)

  ; initial state
  (gallery/init size))


(defn- draw-gallery[state]
  (quil/background 255)
  (gallery/draw state))


(defn- key-pressed-gallery [state key]
(prn "KeyCode:" (:key-code key))
  (case (:key-code key)
    10  (gallery/key-pressed state :ok)
    27  (gallery/key-pressed state :esc)
    82  (gallery/key-pressed state :redo)
    83  (gallery/key-pressed state :save)
    90  (gallery/key-pressed state :undo)
        (gallery/key-pressed state (:key key))))

;;
;; main
;;
(defn init-frame-gallery [size]
  (quil/defsketch GeoRepl
    :size [600 600]
    :title "GeoRepl Gallery - Select A Drawing"
    :setup (fn [](setup-gallery size))
    :draw draw-gallery
    :update gallery/update-frame
    :mouse-pressed gallery/mouse-pressed
    :mouse-released gallery/mouse-released
    :mouse-moved gallery/mouse-moved
    :key-pressed key-pressed-gallery
    :on-close gallery/on-close
    :middleware [m/fun-mode])
  nil)

