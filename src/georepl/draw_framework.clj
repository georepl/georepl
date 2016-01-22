(ns georepl.draw-framework
  (:require [clojure.core.match :refer [match]]
            [quil.core :as quil]
            [georepl.mathlib :as math]
            [georepl.draw-primitives :as dp]
            [quil.middleware :as m]
            [georepl.paint :as paint]))



;;
;; framework functions
;;
(defn setup []
  (quil/background 255)
  (quil/no-fill)
(quil/frame-rate 5)

  ; initial state
  (paint/init))


(defn draw [state]
  (quil/background 255)
  (paint/draw state))

(defn key-pressed [state key]
  (if (= (:key-code key) 10)
    (paint/key-pressed state :ok)
    (paint/key-pressed state (:key key))))

;;
;; main
;;
(defn init-frame []
  (quil/defsketch GeoRepl
    :size [800 800]
    :setup setup
    :draw draw
    :update (paint/wrap paint/update-frame)
    :mouse-pressed (paint/wrap paint/mouse-pressed)
    :mouse-released (paint/wrap paint/mouse-released)
    :mouse-dragged (paint/wrap paint/mouse-dragged)
    :mouse-moved (paint/wrap paint/mouse-moved)
    :key-pressed key-pressed
    :middleware [m/fun-mode])
  nil)

(init-frame)

