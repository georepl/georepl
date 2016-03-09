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
      (nearly-zero? (reduce + (map (comp #(if (pos? %) % (* -1 %)) -) x y)))
      (nearly-zero? (- x y))))
  ([x y & more]
    (and (equals? x y)
      (if (next more)
        (equals? y (first more) (rest more))
        (equals? y (first more))))))

(defn vec-sub [p q]
  (vec (map - q p)))

(defn vec-add [p q]
  (vec (map + q p)))

(defn vec-scal-mult [a v]
  (vec (map (partial * a) v)))

(defn vec-zero? [v]
  (every? nearly-zero? v))

(defn vec-equals? [v w]
  (vec-zero? (map - v w)))

(defn vec-not-equals?[p q]
  (not (vec-equals? p q)))

(defn vec-ortho [v]
  "return a vector which is orthogonal to the given vector v"
  [(* -1.0 (second v)) (first v)])

(defn length
  [p]
  (sqrt (reduce + (map * p p))))

(defn dist [p q]
  (length (vec-sub p q)))

(defn vec-scale [p-fix p-var factor]
  "return new p-var which is (* factor (len v)) apart from p-fix in the original direction"
  (vec-add
    p-fix
    (vec-scal-mult factor
      (vec-sub p-fix p-var))))

(defn dot-product [v w]
  (reduce + (map * v w)))

(defn angle
  ([v]
   (let [len (length v)]
     (if (nearly-zero? len)
       nil
       (if (>= (/ (second v) len) 0)
         (acos (/ (first v) len))
         (- TWO-PI (acos (/ (first v) len)))))))
  ([p q]
    (let [psi (angle p)
          phi (angle q)]
      (if (or (nil? phi)(nil? psi))
        nil
       (- phi psi)))))


(defn angle-dir
  ([v w]
    (let [len (* (length v)(length w))]
      (if (nearly-zero? len)
        nil
        (acos (/ (dot-product v w) len)))))
  ([v]
    (angle v [1 0])))


(defn vec-rotate-center [p angle]
  (let [c (cos angle)
        s (sin angle)
        [x y] p]
    (vec (list (- (* c x)(* s y))
               (+ (* c y)(* s x))))))

(defn vec-rotate [p p-center angle]
  (vec-add
    p-center
    (vec-rotate-center
      (vec-sub p-center p)
      angle)))

(defn project
  [p p-center radius]
  (if (or
        (equals? radius 0.0)
        (nearly-zero? (dist p p-center)))
    p-center
    (vec-scale p-center
               p
               (/ radius
                  (dist p-center p)))))

(defn det
  [v w]
  (let [[a b] v
        [c d] w]
    (- (* a d)(* c b))))

(defn right-from?
  "is point q on the right-hand side of the line from p1 to p2?"
  ([p1 p2 q]
    (let [v (vec-sub p1 p2)
          w (vec-sub p1 q)]
      (neg? (det v w))))
  ([[p1 p2] q]
    (right-from? p1 p2 q)))


; [0 2PI] -> [0 360[
(defn degree
  [angle]
  (* (/ angle TWO-PI) 360.0))


(defn project-point-onto-circle
  [p center-p radius]
  (let [v (vec-sub center-p p)]
    (vec-add (vec-scal-mult (/ radius
                                  (length v))
                               v)
             center-p)))


(defn intersect-circles [p-center1 r1 p-center2 r2]
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

        py (+ y1 (/ (+ (* b d) (* a num-factor)) denominator))
        qy (+ y1 (/ (+ (* b d) (* a num-factor)) denominator))]
    [[px py][qx qy]]))



(defn intersect-lines
  [[[x1 y1][x2 y2]][[x3 y3][x4 y4]]]
  (let [divisor (- (* (- y4 y3)(- x2 x1))
                   (* (- y2 y1)(- x4 x3)))]
    (if (nearly-zero? divisor)
      nil
      [(/ (- (* (- x4 x3)
                (- (* x2 y1) (* x1 y2)))
             (* (- x2 x1)
                (- (* x4 y3) (* x3 y4))))
          divisor)

       (/ (- (* (- y1 y2)
                (- (* x4 y3)(* x3 y4)))
             (* (- y3 y4)
                (- (* x2 y1) (* x1 y2))))
          divisor)])))


(defn intersect-line-segments
  [[[x1 y1][x2 y2]][[x3 y3][x4 y4]]]
  (let [s (intersect-lines [[x1 y1][x2 y2]][[x3 y3][x4 y4]])]
    (if (nil? s)
      nil
      (let [[x y] [(first s)(second s)]]
        (if (and (< (min x1 x2) x (max x1 x2))
                 (< (min x3 x4) x (max x3 x4))
                 (< (min y1 y2) y (max y1 y2))
                 (< (min y3 y4) y (max y3 y4)))
           [x y]
           nil)))))

(defn circumcircle [a b c]
  "return center and radius of the circumscribed circle of the triangle given as points a, b, c"
  (let [v1 (vec-sub b a)
        v2 (vec-sub b c)
        p1 (vec-add b (vec-scal-mult 0.5 v1))
        p2 (vec-add b (vec-scal-mult 0.5 v2))
        q1 (vec-add p1 (vec-ortho v1))
        q2 (vec-add p2 (vec-ortho v2))
        center (intersect-lines [p1 q1][p2 q2])]
    (if (nil? center)
      nil
      [center (dist b center)])))

(defn our-round
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn oriented-curvature?[coll]
  (not (and (some neg? coll) (some pos? coll))))


;;NYI: REFACTOR: delete
(defn disjoin-plus-minus
  [coll]
  (let [cl (filter (comp not nearly-zero?) coll)]
    (if (empty? cl)
      [[0] [0]]
      [(filter pos? cl) (filter neg? cl)])))


(defn proximity
  [coll delta]
  (let [x-min (reduce min (map first coll))
        x-max (reduce max (map first coll))
        y-min (reduce min (map last  coll))
        y-max (reduce max (map last  coll))]
    (if (or
          (> (- x-max x-min) delta)
          (> (- y-max y-min) delta))
      nil
      [(/ (+ x-max x-min) 2)
       (/ (+ y-max y-min) 2)])))
