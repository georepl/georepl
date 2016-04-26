(ns georepl.main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [georepl.draw-framework :as frame]
            [georepl.shapes :as shapes]
            [georepl.mathlib :as math]
            [georepl.elements :as elements]
            [georepl.repl :as repl]
            [georepl.configuration :as config]))


;; read drawing from a file
(defn- file2drawing [filename]
  (let [drw (read-string
              (slurp (apply str (concat
                                  (:drawings-directory config/Configuration)
                                  filename))))
        p-ref (math/vec-scal-mult 0.5 (:size drw))]
    (assoc drw :filename filename
               :p-ref p-ref)))

;; scale a drawing so it fits in a given icon box
(defn- drawing2size [drawing icon-box-size]
  (let [[cur-x cur-y] (:size drawing)
        ratio (max (/ (first icon-box-size) cur-x)(/ (second icon-box-size) cur-y))
        frame [(shapes/constructText (:filename drawing) [0 cur-y][cur-x (* 1.1 cur-y)])
               (shapes/constructLine [0 0] [cur-x 0])
               (shapes/constructLine [cur-x 0] [cur-x cur-y])
               (shapes/constructLine [cur-x cur-y][0 cur-y])
               (shapes/constructLine [0 cur-y][0 0])]
               frm (assoc (shapes/constructCompound frame)
                     :p-ref (math/vec-scal-mult 0.5 (:size drawing))
                     :subtype :frame)]
    (shapes/scale
      (assoc drawing :elems (cons frm (:elems drawing)))
        ratio)))


;; order a list of icons to fit in a box of the given size
(defn- order-icons [icons size]
  (let [icon-len (* 0.9 0.25 (first (:size (first icons))))  ;; scale drawing to 0.25 of its original size and leave some margin (10%)
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


;; helper to find a number as part of a string
(defn- str2int [s]
  (let [sNum (re-find #"[0-9]*" s)]
    (if (empty? sNum)
      nil
     (Integer/parseInt sNum))))


;; create a filename which doesn't exist yet. The file name pattern is <root from config><first new number>.grl
(defn- new-unique-filename [files]
  (let [name-root (:new-drawing-name-root config/Configuration)
        temp-files (filter #(str/starts-with? % name-root) files)]
    (if (empty? temp-files)
      (format "%s0.grl" name-root)
      (let [len (count name-root)
            s-append-list (map #(apply str (drop len %)) temp-files)]
        (if (empty? s-append-list)
          (format "%s0.grl" name-root 1)
          (let [coll (keep str2int s-append-list)
                free-num (last
                           (first
                             (drop-while
                               (comp not first)
                               (map #(list (not-any? (partial = %) coll) %) (range)))))]
            (format "%s%d.grl" name-root free-num)))))))


;; create an empty drawing to start with
(defn- create-new-drawing [size files]
  (let [filename (new-unique-filename files)]
    (assoc (shapes/constructCompound [])
      :subtype :drawing
      :p-ref (math/vec-scal-mult 0.5 size)
      :filename filename
      :size size)))


;select a drawing from the ones in the working directory (defined in configs)
(defn- select-drawing [all-files size]
  ;; select drawing if available or return nil to start with empty one
  (let [files (filter #(= (take 4 (reverse %)) '(\l \r \g \.)) all-files)]
    (if (empty? files)
      nil
      (let [tmp (create-new-drawing [800 800] files)
            dummy (assoc tmp :filename (format "<%s>" (:filename tmp)))
            drawings (cons
                       dummy
                       (map file2drawing files))                 ;; make dummy (empty drawing) first element
            icon-box-size (math/vec-scal-mult (* 0.9 0.25) size)
            icons (map #(drawing2size % icon-box-size) drawings)
            elems (order-icons icons size)]
         (if (empty? elems)
           nil
           (let [drw (assoc (shapes/constructCompound (vec elems))
                        :subtype :drawing
                        :p-ref (math/vec-scal-mult 0.5 size))]
             (do
                (elements/push-elem drw)
                drw)))))))


;; start a new quil sketch and reinitialize the elements stack with a new (empty) drawing
(defn- start-new-drawing [size files]
  (frame/init-frame-paint)
  (elements/clear)
  (let [[server upd-f](repl/start)]
    (elements/register upd-f))
  (elements/push-elem (create-new-drawing size files)))


;; Start paint frame with the selected drawing. This code is injected into draw-framework
;; and is started when the galery frame is done (synchronization).
(defn- start-existing-drawing [filename]
  (let [size [800 800]
        files (.list (io/file (:drawings-directory config/Configuration)))]
    (if (some (or (partial = \<)(partial = \>)) filename)
      (start-new-drawing size files)
      (let [drw (file2drawing filename)]
        (elements/push-elem drw)
        (frame/init-frame-paint)
        (repl/start)))))


(defn- main[]
  (let [size [600 600]
        files (.list (io/file (:drawings-directory config/Configuration)))]
    (if-let [drw-list (select-drawing files size)]
      ;; start gallery
      (frame/init-frame-gallery drw-list start-existing-drawing)
      (start-new-drawing size files))))



;; start the show ...
(main)
