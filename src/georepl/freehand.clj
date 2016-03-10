(ns georepl.freehand
  (:require [georepl.mathlib :as math]
            [georepl.shapes :as shapes]))


;; these are going to be individual user values someday ...
;;
(def short-range 5)
(def snap-duration 1000)
(def dash-speed 0.9)


;;
;; helper functions
;;

(defn timestamp [p]
  (last p))

(defn coordinates [p]
  [(first p)(second p)])

(defn dash-velocity [trace]
  (if (< (count trace) 2)
    0.0
    (let [v (math/vec-sub (last trace)(first trace))]
      (/ (math/length (coordinates v))
         (max 1 (math/abs (timestamp v)))))))


; take a random subset of the contour including start and end points in order to avoid pixel-driven Manhattan geometry
(defn distribute-points [coll]
  (let [len     (count coll)
        divisor (last (take-while (partial > len) [0 2 3 10]))
        runs    (partition-all (int (/ len (max divisor 1))) coll)]
    (dedupe (cons (first coll) (map last runs)))))



; project first, second and two points 'in the middle' of the contour onto the given circle.
; Why two points? To determine orientation! Return projections of first and last point in
; mathematical orientation or [0 0] if arc is a full circle"
(defn arc-segment [p-center radius elm-list]
  (let [[c1 c2] (split-at (int (/ (count elm-list) 2))
                          (map coordinates elm-list))
        coll [(first c1)(last c1)(first c2)(last c2)]
        [p1 p2 p3 p4] (map #(math/project-circle %
                                                 p-center
                                                 radius) coll)
        b1 (math/right-from? p1 p4 p2)
        b2 (math/right-from? p2 p3 p-center)]
    (if (= b1 b2)
      [[0 0][0 0]] ; full circle
      (if b1
        [p1 p4]    ; deliver triangle in mathematical orientation
        [p4 p1]))))



;; helpers (statistics)
;;
(defn average
  [coll]
    (/(reduce + coll)(max 1 (count coll))))

(defn bias
  [p points]
  (average (map (comp math/sq (partial - p)) points)))



;; create geometric objects from drawn input
;;
(defn analyze-shape [elm-list]
  (if (< (count elm-list) 2)
    nil
    (if (= (count elm-list) 2)
      (shapes/constructLine (coordinates (first elm-list))(coordinates (first elm-list)))
      (let [elems (dedupe (map coordinates (distribute-points elm-list)))
            v-diff  (map math/vec-sub (rest elems) elems)
            v-mean  (math/vec-sub (last elems)(first elems))
            angles  (map #(math/angle v-mean %) v-diff)
            avg-an  (average angles)
            bias-an (bias avg-an angles)
            [c-pos c-neg] (math/disjoin-plus-minus (map math/det v-diff (rest v-diff)))

;; NYI: REFACTOR: use something like curvature here
            curve-ratio (float (/ (min (count c-pos) (count c-neg))
                                  (max (count c-pos) (count c-neg))))]
       (if (and (< bias-an 0.15)(< (math/abs avg-an) 0.2))
         (if (> (dash-velocity elm-list) dash-speed)
           (assoc (shapes/constructLine (last elems) (first elems) 0) :type :dashed)
           (shapes/constructLine (last elems) (first elems)))
         (if (< curve-ratio 0.25)
           (let [xmin (reduce min (map first elems))
                   ymin (reduce min (map second elems))
                 xmax (reduce max (map first elems))
                 ymax (reduce max (map second elems))
                 p-center [(math/round (/ (+ xmax xmin) 2))(math/round (/ (+ ymax ymin) 2))]
                 radius (/ (+ (- xmax xmin)(- ymax ymin)) 4)
                 [p-start p-end] (arc-segment p-center radius elm-list)]
             (if (math/equals? p-start p-end [0.0 0.0])
                 (shapes/constructCircle p-center radius)
                 (shapes/constructArc p-center radius p-start p-end)))
           (shapes/constructContour elm-list)))))))

