(ns georepl.main
  (:require [georepl.draw-framework :as frame]
            [georepl.shapes :as shapes]
            [georepl.elements :as elements]))


(defn main[]
  ;; select drawing if available or start with empty one


  ;; initialize drawing compound with empty shapes list
  (elements/push-elem
    (assoc (shapes/constructCompound []) :subtype :drawing))


  ;; start drawing
  (frame/init-frame))


;; start the show ...
(main)
