(ns georepl.paint-test
  (:require [clojure.test :refer :all]
            [georepl.shapes :as shapes]
            [georepl.mathlib :as math]
            [georepl.paint :as paint]))



(deftest snap-time-exceeded-test
  (let [t (System/currentTimeMillis)]
    (testing "not exceeded"
      (is (false? (#'paint/snap-time-exceeded? t))))
    (Thread/sleep 2000)
    (testing "exceeded"
      (is (true? (#'paint/snap-time-exceeded? t))))))

(deftest next-point-on-element-test
  (testing "next-point-on-element"
    (let [coll (list (shapes/constructPoint [150 250])
                     (shapes/constructLine [400 350][250 60])
                     (shapes/constructCircle [450 450] 80)
                     (shapes/constructArc [200 450] 30 [230 450][200 480])
                     (shapes/constructContour [[100 350][50 450][150 400][200 350]]))
          [e p d] (#'paint/next-point-on-element coll [300 300])]
      (is (:type e) :line)
      (is (math/equals? [400 350] p))
      (is (math/equals? 111.80339 d))
      )))

;; weird: simply commenting this out using "comment" is not sufficiant for lein cloverage!
;;(comment
;;(deftest reset-state-test
;;  (let [state ((paint/wrap paint/reset-state) (paint/->Drawing))]
;;    (testing "reset-state"
;;      (is (and (nil? (:button-down-time state))
;;               (empty? (:trace state))
;;               (false? (:show-trace? state))
;;               (false? (:complete? state)))))
;;  (testing "draw-temporary with empty point list"
;;    (is (false? (:complete? (paint/draw-temporary (assoc state :show-trace? true)))))
;;    (let [state1 (paint/draw-temporary (assoc state :show-trace? true
;;                                                    :trace [[4 6][3 7][5 8][7 4][8 2][6 5]]))]
;;      (is true)))
;;))
;;)
