(ns georepl.main
  (:require [clojure.java.io :as io]
            [georepl.draw-framework :as frame]
            [georepl.shapes :as shapes]
            [georepl.mathlib :as math]
            [georepl.elements :as elements]))


(defn- file2drawing [filename]
  (let [drw (read-string
              (slurp (apply str (concat "workbench/" filename))))
        p-ref (math/vec-scal-mult 0.5 (:size drw))]
    (assoc drw :filename filename
               :p-ref p-ref)))


(defn- drawing2size [drawing icon-box-size]
  (let [[cur-x cur-y] (:size drawing)
        ratio (max (/ (first icon-box-size) cur-x)(/ (second icon-box-size) cur-y))
        frame [(shapes/constructText (:filename drawing) [0 cur-y][cur-x (* 1.1 cur-y)])
               (shapes/constructLine [0 0] [cur-x 0])
               (shapes/constructLine [cur-x 0] [cur-x cur-y])
               (shapes/constructLine [cur-x cur-y][0 cur-y])
               (shapes/constructLine [0 cur-y][0 0])]
               frm (assoc (shapes/constructCompound frame) :p-ref (math/vec-scal-mult 0.5 (:size drawing)) :subtype :frame)]
    (shapes/scale
      (assoc drawing :elems (cons frm (:elems drawing)))
         ratio)))

;;
(defn- order-icons [icons size]
  (let [icon-len (* 0.9 0.25 (first (:size (first icons))))
        icons-per-line (int (/ (first size) (max icon-len 1)))
        dist (/ (first size) icons-per-line)
        p-center-list (map
                        #(math/vec-add
                           (math/vec-scal-mult 0.5 [dist dist])
                           (math/vec-scal-mult dist [(mod % icons-per-line)(int (/ % icons-per-line))]))
                             (range (count icons)))]
  (map
    #(shapes/translate %1 (math/vec-sub %2 (:p-ref %1)))
      icons p-center-list)))


(defn- select-drawing [all-files size]
  ;; select drawing if available or start with empty one
  (let [files (filter #(= (take 4 (reverse %)) '(\l \r \g \.)) all-files)
        drawings (map file2drawing files)
        icon-box-size (math/vec-scal-mult (* 0.9 0.25) size)
        icons (map #(drawing2size % icon-box-size) drawings)
        elems (order-icons icons size)]
;(prn "SelectDrawing:" (:subtype (first (:elems (first elems)))))
     (if (empty? elems)
      nil
      (elements/push-elem
       (assoc (shapes/constructCompound (vec elems))
         :subtype :drawing
         :p-ref (math/vec-scal-mult 0.5 size))))))


(defn- create-new-drawing[size]
  ;; NYI: create unique new name
  (let [filename "workbench/test.grl"]
    (elements/push-elem
      (assoc (shapes/constructCompound [])
        :subtype :drawing
        :p-ref (math/vec-scal-mult 0.5 size)
        :filename filename
        :size size))))


(defn- main[]
  (let [size [600 600]]
    (let [files (.list (io/file "workbench"))]
      (if-let [drw-list (select-drawing files size)]
(do
  (prn drw-list)
        ;; start gallery
        (frame/init-frame-gallery size)
)
        (create-new-drawing size)))

    ;; start drawing
;    (frame/init-frame-paint size)
))


;; start the show ...
(main)
