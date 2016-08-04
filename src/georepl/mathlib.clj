(ns georepl.mathlib)


(def EPS 0.00001)  ; smallest possible float to prevent division-by-zero!
(def PI Math/PI)
(def TWO-PI (* 2 Math/PI))
(def PI-HALF (/ Math/PI 2))


(defn abs[x] (Math/abs x))
(defn round[x] (Math/round (double x)))
(defn sq[x] (* x x))
(defn sqrt[x] (Math/sqrt x))
(defn acos[x] (Math/acos x))
(defn asin[x] (Math/asin x))
(defn cos[x] (Math/cos x))
(defn sin[x] (Math/sin x))
(defn sgn[x] (if (>= x 0.0) 1 -1))


(defn coordinates [p]
  [(first p)(second p)])

(defn nearly-zero?
  ([x epsilon]
    (let [eps (abs epsilon)]
      (if (zero? eps)
        (nearly-zero? x)
        (< (* -1.0 eps) x eps))))
  ([x]
    (nearly-zero? x EPS)))

(defn equals?
  ([x y]
    (if (and (coll? x)(coll? y))
      (every? nearly-zero? (map - x y))
      (nearly-zero? (- x y))))
  ([x y & more]
    (and (equals? x y)
         (every? true? (map (partial equals? x) more)))))

(defn vec-sub [p q]
  (vec (map - p q)))

(defn vec-add [p q]
  (vec (map + q p)))

(defn vec-scal-mult [a v]
  (vec (map (partial * a) v)))

(defn vec-zero? [v]
  (every? nearly-zero? v))

(defn not-equals?[p q]
  (not (equals? p q)))

(defn vec-scale [p-fix p-var factor]
  "return new p-var which is (* factor (len v)) apart from p-fix in the original direction"
  (vec-add
    p-fix
    (vec-scal-mult factor
      (vec-sub p-var p-fix))))

(defn vec-ortho [v]
  "return a vector which is orthogonal to the given vector v"
  [(* -1.0 (second v)) (first v)])

(defn length
  [p]
  (sqrt (reduce + (map * p p))))

(defn dist [p q]
  (length (vec-sub q p)))

(defn dot-product [v w]
  (reduce + (map * v w)))

(defn det [v w]
  (let [[a b] v
        [c d] w]
    (- (* a d)(* c b))))

(defn right-from?
  "is point q on the right-hand side of the line from p1 to p2?"
  ([p1 p2 q]
    (let [v (vec-sub p2 p1)
          w (vec-sub q p1)]
      (neg? (det v w))))
  ([[p1 p2] q]
    (right-from? p1 p2 q)))

(defn angle
  ([v w]
    (let [l (* (length v)(length w))]
      (if (nearly-zero? l)
        0.0
        (let [dp (dot-product v w)
              dpn (if (> (abs dp)(abs l)) l dp)
              phi (acos (/ dpn l))]
          (if (pos? (det v w))
            (* -1 phi)
            phi)))))
  ([v]
    (angle v [1 0])))


;; v := p-p2
;; w := D90° (p2 - p1)
;; p-res = <v,w>/(|w||w|) * w
(defn project-line [p p1 p2]
  (if (equals? p1 p2)
    p1
    (let [v (vec-sub p p2)
          w (vec-ortho (vec-sub p1 p2))
          a (/ (dot-product v w)(dot-product w w))]
      (vec-add p2 (vec-scal-mult a w)))))

(defn on-straight-line? [q p1 p2]
  (let [v (vec-sub p2 p1)
        w (vec-sub q p1)]
    (nearly-zero? (det v w))))

(defn on-line? [q p1 p2]
  (equals? (dist p1 p2) (+ (dist q p1)(dist q p2))))

(defn vec-rotate-center [p angle]
  (let [c (cos angle)
        s (sin angle)
        [x y] p]
    (vec (list (- (* c x)(* s y))
               (+ (* c y)(* s x))))))

(defn vec-rotate [p p-c angle]
  (vec-add
    p-c
    (vec-rotate-center
      (vec-sub p p-c)
      angle)))

(defn project-circle [p p-center radius]
  (if (or
        (equals? radius 0.0)
        (nearly-zero? (dist p p-center)))
    p-center
    (vec-scale p-center
               p
               (/ radius
                  (dist p-center p)))))

; [0 2PI] -> [0 360[
(defn degree [angle]
  (* (/ angle TWO-PI) 360.0))


(defn project-point-onto-circle [p center-p radius]
  (let [v (vec-sub p center-p)]
    (vec-add (vec-scal-mult (/ radius
                               (max EPS (length v)))
                            v)
             center-p)))

(defn on-circle? [q cp radius]
  (equals? (dist cp q) radius))


(defn on-arc? [q cp radius p1 p2]
  (and (on-circle? q cp radius)
       (on-circle? p1 cp radius)
       (on-circle? p2 cp radius)
       (or (right-from? p1 p2 q)(equals? p1 q)(equals? p2 q))))


(defn intersect-circles [p-center1 r1 p-center2 r2]
  (if (or (equals? p-center1 p-center2)
          (nearly-zero? r1)
          (nearly-zero? r2)
          (> (dist p-center1 p-center2) (+ r1 r2)))
    []
    (dedupe
      (let [[x1 y1] (take 2 p-center1)
            [x2 y2] (take 2 p-center2)

            a (* 2 (- x2 x1))
            b (* 2 (- y2 y1))
            c (- (+ (* x2 x2)(* y2 y2)(* r1 r1))
                 (+ (* x1 x1)(* y1 y1)(* r2 r2)))

            d (- c (+ (* a x1)(* b y1)))

            denominator (+ (* a a)(* b b))
            num-factor (sqrt (- ( *(* r1 r1) denominator)(* d d)))

            px (+ x1 (/ (+ (* a d) (* b num-factor)) denominator))
            qx (+ x1 (/ (- (* a d) (* b num-factor)) denominator))

            py (+ y1 (/ (- (* b d) (* a num-factor)) denominator))
            qy (+ y1 (/ (+ (* b d) (* a num-factor)) denominator))]
        [[px py][qx qy]]))))


(defn intersect-circle-arc [p-center radius cp r start end]
  (filter
    #(on-arc? % cp r start end)
    (filter
      (comp not empty?)
        (intersect-circles p-center radius cp r))))

(defn intersect-arcs [cp1 r1 start1 end1 cp2 r2 start2 end2]
  (filter
    #(on-arc? % cp1 r1 start1 end1)
    (intersect-circle-arc cp1 r1 cp2 r2 start2 end2)))


(defn intersect-straight-lines [[x1 y1][x2 y2][x3 y3][x4 y4]]
  (let [divisor (- (* (- y4 y3)(- x2 x1))
                   (* (- y2 y1)(- x4 x3)))]
    (if (nearly-zero? divisor)
      []
      [[(/ (- (* (- x4 x3)
                 (- (* x2 y1) (* x1 y2)))
              (* (- x2 x1)
                 (- (* x4 y3) (* x3 y4))))
           divisor)

        (/ (- (* (- y1 y2)
                 (- (* x4 y3)(* x3 y4)))
              (* (- y3 y4)
                 (- (* x2 y1) (* x1 y2))))
           divisor)]])))


(defn intersect-lines [p1 p2 p3 p4]
  (let [s (intersect-straight-lines p1 p2 p3 p4)]
    (if (and (not (empty? s))(on-line? (first s) p1 p2)(on-line? (first s) p3 p4))
      s
      [])))


(defn intersect-straight-line-circle [[x1 y1] [x2 y2] [cpx cpy] radius]
  (if (nearly-zero? radius)
    (on-straight-line? [cpx cpy] [x1 y1] [x2 y2])
    (dedupe
      (let [a (- y1 y2)
            b (- x2 x1)]
        (if (nearly-zero? b)
          (let [dx (abs (- cpx x1))]
            (if (> dx radius)
              []
              (let [angle (acos (/ dx radius))
                    dy (* radius (sin angle))]
                [[x1 (+ cpy dy)]
                 [x1 (- cpy dy)]])))
          (let [d (+ (* a (- x1 cpx))(* b (- y1 cpy)))
                rr (* radius radius)
                dd (* d d)
                quot (+ (* a a)(* b b))
                disc (- (* rr quot) dd)]
            (if (< disc 0.0)
              []
              (let [ad (* a d)
                    bd (* b d)
                    rt (sqrt disc)]
                [[(+ cpx (/ (+ ad (* b rt)) quot))
                  (+ cpy (/ (- bd (* a rt)) quot))]
                 [(+ cpx (/ (- ad (* b rt)) quot))
                  (+ cpy (/ (+ bd (* a rt)) quot))]]))))))))


(defn intersect-line-circle [p1 p2 cp radius]
  (filter
    #(on-line? % p1 p2)
    (filter
      (comp not empty?)
        (intersect-straight-line-circle p1 p2 cp radius))))


(defn intersect-line-arc [p1 p2 cp radius start end]
  (filter
    #(on-arc? % cp radius start end)
    (filter
      (comp not empty?)
        (intersect-line-circle p1 p2 cp radius))))


(defn circumcircle [a b c]
  "return center and radius of the circumscribed circle of the triangle given as points a, b, c"
  (let [v1 (vec-sub a b)
        v2 (vec-sub c b)
        p1 (vec-add b (vec-scal-mult 0.5 v1))
        p2 (vec-add b (vec-scal-mult 0.5 v2))
        q1 (vec-add p1 (vec-ortho v1))
        q2 (vec-add p2 (vec-ortho v2))
        center (intersect-straight-lines p1 q1 p2 q2)]
    (if (empty? center)
      nil
      [(first center) (dist b (first center))])))


;; get the minimal box containng all points of a given list
;;
(defn box [coord-list]
  (let [coll (apply mapv vector coord-list)]
    [(vec (map #(reduce min %) coll))
     (vec (map #(reduce max %) coll))]))


