(ns georepl.main
  (:require [georepl.draw-framework :as frame]))


(defn main[]
  ;; select drawing if available or start with empty one

  ;; start drawing
  (frame/init-frame))


;; start the show ...
(main)
