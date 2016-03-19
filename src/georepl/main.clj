(ns georepl.main
  (:require [clojure.java.io :as io]
            [georepl.draw-framework :as frame]
            [georepl.shapes :as shapes]
            [georepl.elements :as elements]))


(defn main[]
  ;; select drawing if available or start with empty one
  (let [files (.list (io/file "workbench"))]
(prn (drop-while #(not= % "test.grl") files))
    (if-let [f (first (drop-while #(not= % "test.grl") files))]
      (elements/slurp-drawing "workbench/test.grl")
      ;; initialize drawing compound with empty shapes list
      (elements/push-elem
        (assoc (shapes/constructCompound []) :subtype :drawing
                                             :filename "workbench/test.grl"))))

  ;; start drawing
  (frame/init-frame)
)


;; start the show ...
(main)
