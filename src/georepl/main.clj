(ns georepl.main
  (:require [clojure.java.io :as io]
            [georepl.draw-framework :as frame]
            [georepl.shapes :as shapes]
            [georepl.mathlib :as math]
            [georepl.elements :as elements]))


(comment
(defn _select-drawing [all-files size]
  ;; select drawing if available or start with empty one
;  (frame/init-frame-paint size)
;  (elements/push-elem {:type :compound :subtype :drawing :elems []})

  (let [files (filter #(= (take 4 (reverse %)) '(\l \r \g \.)) all-files)
        p-center-list (repeat [50 400])
        factor 0.25
        elems (map
                #(elements/slurp-drawing (apply str (concat "workbench/" %1))
                   (fn[drw]
                      (shapes/translate
                        (shapes/scale drw factor)
                          %2))) files p-center-list)]
;prn "Main:" (map #(list (:p-ref %)(:type %)) elems))

    (elements/push-elem (assoc (shapes/constructCompound (vec elems))
                          :p-ref (math/vec-scal-mult 0.5 size)))))
)

(defn select-drawing [all-files size]
  ;; select drawing if available or start with empty one
;  (frame/init-frame-paint size)
;  (elements/push-elem {:type :compound :subtype :drawing :elems []})

  (let [files (filter #(= (take 4 (reverse %)) '(\l \r \g \.)) all-files)
        p-center-list (iterate (fn[p](math/vec-add p [200 0])) [50 0])
        factor 0.25
        drawings (map #(read-string (slurp (apply str (concat "workbench/" %1)))) files)
        elems (map #(shapes/translate %1 %2)
                   (map #(shapes/scale % factor) drawings)
                   p-center-list)]
(prn "Testo:" elems)
    (elements/push-elem
      (assoc (shapes/constructCompound (vec elems))
        :subtype :drawing
        :p-ref (math/vec-scal-mult 0.5 size)))))


(defn create-new-drawing[size]
  ;; NYI: create unique new name
  (let [filename "workbench/test.grl"]
    (elements/push-elem
      (assoc (shapes/constructCompound [])
        :p-ref (math/vec-scal-mult 0.5 size)
        :subtype :drawing
        :filename filename))))


(defn main[]
  (let [size [800 800]]
    (let [files (.list (io/file "workbench"))]
      (if-let [f (first (drop-while #(not= % "test.grl") files))]
        (select-drawing files size)
        (create-new-drawing size)))

    ;; start drawing
    (frame/init-frame-paint size)))


;; start the show ...
(main)
