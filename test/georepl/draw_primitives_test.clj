(ns georepl.draw-primitives-test
  (:require [clojure.test :refer :all]
            [georepl.shapes :as shapes]
            [georepl.draw-framework :as fw]
            [georepl.draw-primitives :as dp]))


(deftest exit-test
  (#'fw/init-renderer :test)
  (is (= :exit (get (#'dp/exit) :f-renderer))))

(deftest draw-str-test
  (#'fw/init-renderer :test)
  (is (= :no-fill (get (#'dp/draw-str "Test" 0 0 100 100) :f-renderer))))

(deftest draw-point-test
  (#'fw/init-renderer :test)
  (is (= "argument 42 must be two-dimensional vector" (#'dp/draw-point 42)))
  (is (= :ellipse (get (#'dp/draw-point [42 18]) :f-renderer)))
  (is (= :no-fill (get (#'dp/draw-point [42 18] 8) :f-renderer)))
  (is (= :no-fill (get (#'dp/draw-point [42 18] [100 0 0]) :f-renderer)))
  (is (= :no-fill (get (#'dp/draw-point [42 18] [100 0 0] 42) :f-renderer)))
  )

(deftest draw-element-test
  (is (= nil (#'dp/draw-element (#'shapes/constructCompound [(#'shapes/constructLine [0 0][100 30])(#'shapes/constructCircle [50 15] 10)]) true)))
  (is (= nil (#'dp/draw-element [[0 0][1 0][2 -1][3 0][4 1]] true))))

(def selcoll (list {:s "foo" :p1 [0 10] :p2 [25 15] :f #(42) :create :polyline :highlight true}
                   {:s "bizz" :p1 [0 15] :p2 [25 20] :f #(43) :create :point :highlight false}
                   {:s "buzz" :p1 [0 20] :p2 [25 25] :f #(44) :create :modify :highlight false}))

(deftest draw-text-vec-test
  (#'fw/init-renderer :test)
  (is (= :no-fill (get (#'dp/draw-text-vec selcoll) :f-renderer)))
  (is (nil? (get (#'dp/draw-text-vec (map #(dissoc % :p1) selcoll)) :f-renderer)))
  (is (nil? (get (#'dp/draw-text-vec (map #(dissoc % :p2) selcoll)) :f-renderer))))
