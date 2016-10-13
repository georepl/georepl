(ns georepl.gui-base-test
  (:require [clojure.test :refer :all]
            [georepl.mathlib :as math]
            [georepl.shapes :as shapes]
            [georepl.elements :as elements]
            [georepl.draw-framework :as fw]
            [georepl.gui-base :as gui-base]))

(def tr1 [[2 3 9 :left][5 1 8 :left][4 2 7 :left][1 4 5 :left][1 3 3 :left][4 2 2 :left]])
(def tr2 [[2 3 200 :right][5 1 300 :left][4 2 500 :left][1 4 700 :right][1 3 800 :right][4 2 900 :left]])
(def tr3 [[2 3 2 :left][5 1 3 :left][4 2 5 :left][1 4 7 :right][1 3 8 :right][4 2 9 :left]])
(def tr4 [[2 3 200 :right][5 1 300 :left][4 2 500 :left][1 4 700 :right][1 3 800 :right][4 2 900 :left][4 2 1900 :left]])

(def drw (#'shapes/constructCompound [] :subtype :drawing :filename "/testfiles/temp.grl"))


(deftest key-pressed-test
  (#'fw/init-renderer :test)
  (#'elements/clear)
  (#'elements/push-elem drw)

  (testing "key-pressed variations without selectable menue"
    (let [state (#'gui-base/init)
          state-without (dissoc state :selection)]
      (is (= state-without (#'gui-base/key-pressed state-without :up)))
      (is (= state-without (#'gui-base/key-pressed state-without :down)))
      (is (= state-without (#'gui-base/key-pressed state-without :ok)))))

  (testing "key-pressed variations with selectable menue"
    (let [state (#'gui-base/init)
          state-u1 (#'gui-base/key-pressed state :up)
          state-u2 (#'gui-base/key-pressed state-u1 :up)
          state-u3 (#'gui-base/key-pressed state-u2 :up)
          state-u4 (#'gui-base/key-pressed state-u3 :up)
          state-d1 (#'gui-base/key-pressed state :down)
          state-d2 (#'gui-base/key-pressed state-d1 :down)
          state-d3 (#'gui-base/key-pressed state-d2 :down)
          state-d4 (#'gui-base/key-pressed state-d3 :down)]
      (is (= [true true false false false] (map :highlight (:selection state-u4))))
      (is (= [false true true false false] (map :highlight (:selection state-u3))))
      (is (= [false false true true false] (map :highlight (:selection state-u2))))
      (is (= [false false false true true] (map :highlight (:selection state-u1))))
      (is (= [true true false false false] (map :highlight (:selection state-d1))))
      (is (= [false true true false false] (map :highlight (:selection state-d2))))
      (is (= [false false true true false] (map :highlight (:selection state-d3))))
      (is (= [false false false true true] (map :highlight (:selection state-d4))))
      (is (= state-d1 (#'gui-base/key-pressed state-d1 :ok)))
      (is (= state-d4 (#'gui-base/key-pressed state-d4 :ok)))
      (is (= state-u3 (#'gui-base/key-pressed state-u3 :ok)))
      ))

  (testing "key-pressed the other stuff"
    (let [state (#'gui-base/init)]
      (is (= state (#'gui-base/key-pressed state :save)))
      (is (= state (#'gui-base/key-pressed state :undo)))
      (is (= state (#'gui-base/key-pressed state :undo)))
      (is (= state (#'gui-base/key-pressed state :redo)))
      (is (= (assoc state :raw-traces true) (#'gui-base/key-pressed state :shift)))
      (is (= state (#'gui-base/key-pressed state :any-unknown-key)))
      )))


(deftest helpers-test
  (testing "trace-length"
    (is (math/equals? 12.78759 (#'gui-base/trace-length tr1)))
    (is (math/equals? 12.78759 (#'gui-base/trace-length tr2)))
    (is (math/equals? 12.78759 (#'gui-base/trace-length tr3)))
    (is (math/equals? 12.78759 (#'gui-base/trace-length tr4)))
    )
  (testing "button-down-time"
    (is (false? (#'gui-base/button-down-time-exceeded? tr1)))
    (is (false? (#'gui-base/button-down-time-exceeded? tr2)))
    (is (false? (#'gui-base/button-down-time-exceeded? tr3)))
    (is (false? (#'gui-base/button-down-time-exceeded? tr4)))
    (is (true? (#'gui-base/button-down-time-exceeded? (reverse tr4))))
    ))

(defn- cleanup [state]
  "dissoc everything from state which is unnecessary  here. Use only for result evaluation!"
  (dissoc state :redo-stack :selection :context-menu :attrbs
                :factory :selection-save))

(deftest mouse-pressed-mouse-released-test
  (#'fw/init-renderer :test)
  (let [state (#'gui-base/init)]
    (testing "init"
      (is (and (zero? (:button-released state))
               (false? (:mouse-moved? state))))
      (is (= state (#'gui-base/update-frame state))))

    (testing "mouse-pressed/mouse-released/update-frame"
      (let [state-mp1 (#'gui-base/mouse-pressed state {:x 100 :y 200 :button :left})]
        (is (= [[100 200]] (map (partial take 2) (:trace state-mp1))))
        (is (false? (:show-context? state-mp1)))
        (is (= -1 (:button-released state-mp1)))
        (is (false? (:mouse-moved? state-mp1)))
        (is (= state-mp1 (#'gui-base/update-frame state-mp1)))
        (let [state-mr1 (#'gui-base/mouse-released state-mp1 {:x 98 :y 204})]
          (is (= [[98 204][100 200]] (map (partial take 2) (:trace state-mr1))))
          (is (false? (:show-context? state-mr1)))
          (is (= 1 (:button-released state-mr1)))
          (is (false? (:mouse-moved? state-mr1)))
          (let [state-mr2 (#'gui-base/update-frame state-mr1)]
            (is (empty? (:trace state-mr2)))
            (is (nil? (:show-trace? state-mr2)))
            (cleanup state-mp1) (cleanup (#'gui-base/update-frame state-mr1)))
      (let [state-mp2 (#'gui-base/mouse-pressed state {:x 100 :y 200 :button :right})]
        (is (empty? (:trace state-mp2)))
        (is (true? (:show-context? state-mp2)))
        (is (= 0 (:button-released state-mp2)))
        (is (false? (:mouse-moved? state-mp2)))
        (is (= state-mp2 (#'gui-base/update-frame state-mp2)))
        ))))
    (testing "show-context situation"
      (let [state-mp1 (#'gui-base/mouse-pressed (assoc state :show-context? true) {:x 100 :y 200 :button :left})]
        (is (= [[100 200]] (map (partial take 2) (:trace state-mp1))))
        (is (true? (:show-context? state-mp1)))
        (is (= 0 (:button-released state-mp1)))
        (is (false? (:mouse-moved? state-mp1)))
        (is (= (#'gui-base/context (:f-context state-mp1) (dissoc state-mp1 :f-context))
               (#'gui-base/update-frame state-mp1)))
        (let [state-mr1 (#'gui-base/mouse-released state-mp1 {:x 98 :y 204})]
          (is (= [[98 204][100 200]] (map (partial take 2) (:trace state-mr1))))
          (is (true? (:show-context? state-mr1)))
          (is (= 0 (:button-released state-mr1)))
          (is (false? (:mouse-moved? state-mr1)))
          (let [state-mr2 (#'gui-base/update-frame state-mr1)]
            (is (empty? (:trace state-mr2)))
            (is (false? (:show-trace? state-mr2)))
            (cleanup state-mp1) (cleanup (#'gui-base/update-frame state-mr1)))
          (let [state-mp2 (#'gui-base/mouse-pressed state {:x 100 :y 200 :button :right})]
            (is (empty? (:trace state-mp2)))
            (is (true? (:show-context? state-mp2)))
            (is (= 0 (:button-released state-mp2)))
            (is (false? (:mouse-moved? state-mp2)))
            (is (= state-mp2 (#'gui-base/update-frame state-mp2)))))))
    (testing "edge cases"
      (let [state-1 (assoc state :show-context? true :selection nil)
            state-mp3 (#'gui-base/mouse-pressed state-1 {:x 100 :y 200 :button :left})]
        (is (= (dissoc state-1 :show-context?) (assoc (dissoc state-mp3 :show-context?) :button-released 0)))
        (is (false? (:show-context? state-mp3)))
        (is (= 0 (:button-released state-mp3)))
        )
      (let [state-2 (dissoc state :button-released)
            state-mp4 (#'gui-base/mouse-released state-2 {:x 100 :y 200 :button :left})]
        (is (= state-2 (dissoc state-mp4 :trace :button-released)))
        (is (= 1 (:button-released state-mp4)))
        (is (= 1 (count (:trace state-mp4))))
        (is (= [100 200] (take 2 (first (:trace state-mp4)))))
        ))))

(deftest mouse-dragged-test
  (#'fw/init-renderer :test)
  (testing "mouse-dragged-uninterrupted-by-update"
    (let [state (#'gui-base/init)
          st1 (#'gui-base/mouse-dragged state {:x 100 :y 200 :button :right})
          st2 (#'gui-base/mouse-dragged st1 {:x 110 :y 190 :button :right})
          st3 (#'gui-base/mouse-dragged st2 {:x 119 :y 182 :button :right})
          st4 (#'gui-base/mouse-dragged st3 {:x 122 :y 175 :button :right})
          st5 (#'gui-base/mouse-dragged st4 {:x 134 :y 167 :button :right})
          states (list st1 st2 st3 st4 st5)]
      (is (every? false? (map :show-trace? states)))
      (is (every? false? (map :mouse-moved? states)))
      (is (every? zero? (map :button-released states)))
      (is (every? false? (map :show-context? states)))
      (is (= [[100 200][110 190][119 182][122 175][134 167]]
             (reverse (map (partial take 2) (:trace st5)))))))


  (testing "mouse-dragged-interrupted-by-update"
    (let [state (#'gui-base/init)
          st1 (#'gui-base/mouse-dragged state {:x 100 :y 200 :button :right})
          st2 (#'gui-base/update-frame st1)
          st3 (#'gui-base/mouse-dragged st2 {:x 110 :y 190 :button :right})
          st4 (#'gui-base/update-frame st3)
          st5 (#'gui-base/mouse-dragged st4 {:x 119 :y 182 :button :right})
          st6 (#'gui-base/update-frame st5)
          st7 (#'gui-base/mouse-dragged st6 {:x 122 :y 175 :button :right})
          st8 (#'gui-base/mouse-dragged st7 {:x 134 :y 167 :button :right})
          st9 (#'gui-base/update-frame st8)
          states (list st1 st2 st3 st4 st5 st6 st7 st8 st9)]
      (is (every? false? (map :show-trace? states)))
      (is (every? false? (map :mouse-moved? states)))
      (is (every? zero? (map :button-released states)))
      (is (every? false? (map :show-context? states)))
      (is (= [[100 200][110 190][119 182][122 175][134 167]]
             (reverse (map (partial take 2) (:trace st9))))))))

(deftest dashed-test
  (let [state (#'gui-base/init)]
    (is (= (assoc state :trace []) (#'gui-base/dashed state)))
    (is (= (assoc state :trace []) (#'gui-base/dashed (assoc state :trace [[10 42 1234567 :left][14 33 1234556 :left][15 31 1234545 :left][17 29 1234534 :left]]))))
    ))


(def state-create #georepl.gui.Creating{:redo-stack [], ,
                                        :attrbs {:show-pointlist? false, :show-context? false, :text-visible? true},
                                        :show-pointlist? false,
                                        :show-context? false,
                                        :text-visible? true,
                                        :back-to-drawing? false,
                                        :trace [], :mouse-moved? false, :button-released 0})

(deftest snapped-test
  (#'fw/init-renderer :test)
  (#'elements/clear)
  (let [state (#'gui-base/init)
        new-state (#'gui-base/snapped (assoc state :trace [[10 42 1234567 :left][12 43 1234566 :left][13 41 1234565 :left][15 39 1234564 :left]]))]
    (is (= (assoc state :trace []) new-state))))

(deftest dragging-test
  (#'fw/init-renderer :test)
  (let [state (#'gui-base/init)]
    (is (= (assoc state :show-trace? true) (#'gui-base/dragging state)))
    ))

(deftest dragged-test
  (let [state (#'gui-base/init)
        new-st #georepl.gui.Creating
                 {:redo-stack [] :factory {:type :none, :elem nil} :show-pointlist? false :show-context? false
                  :text-visible? true :back-to-drawing? false :context-menu nil :trace [] :button-released 0 :mouse-moved? false}
        new-state (assoc new-st :selection-save (:selection state) :selection (:selection state) :attrbs (:attrbs state))]
    (is (= new-state (#'gui-base/dragged state nil)))
    (is (= new-state (#'gui-base/dragged state {:type :point :p [100 100] :p-ref [100 100] :name "Pnt0"})))
    ))

(deftest mouse-moved-test
  (#'fw/init-renderer :test)
  (testing "mouse-moved-uninterrupted-by-update"
    (let [state (#'gui-base/init)
          st1 (#'gui-base/mouse-moved state {:x 100 :y 200 :button :right})
          st2 (#'gui-base/mouse-moved st1 {:x 110 :y 190 :button :right})
          st3 (#'gui-base/mouse-moved st2 {:x 119 :y 182 :button :right})
          st4 (#'gui-base/mouse-moved st3 {:x 122 :y 175 :button :right})
          st5 (#'gui-base/mouse-moved st4 {:x 134 :y 167 :button :right})
          states (list st1 st2 st3 st4 st5)]
      (is (every? false? (map :show-trace? states)))
      (is (every? true? (map :mouse-moved? states)))
      (is (every? neg? (map :button-released states)))
      (is (every? false? (map :show-context? states)))
      (is (= [[100 200][110 190][119 182][122 175][134 167]]
             (reverse (map (partial take 2) (:trace st5)))))))


  (testing "mouse-moved-interrupted-by-update"
    (let [state (#'gui-base/init)
          st1 (#'gui-base/mouse-moved state {:x 100 :y 200 :button :right})
          st2 (#'gui-base/update-frame st1)
          st3 (#'gui-base/mouse-moved st2 {:x 110 :y 190 :button :right})
          st4 (#'gui-base/update-frame st3)
          st5 (#'gui-base/mouse-moved st4 {:x 119 :y 182 :button :right})
          st6 (#'gui-base/update-frame st5)
          st7 (#'gui-base/mouse-moved st6 {:x 122 :y 175 :button :right})
          st8 (#'gui-base/mouse-moved st7 {:x 134 :y 167 :button :right})
          st9 (#'gui-base/update-frame st8)
          states (list st1 st2 st3 st4 st5 st6 st7 st8 st9)]
      (is (every? false? (map :show-trace? states)))
      (is (= '(true false true false true false true true false)
              (map :mouse-moved? states)))
      (is (every? neg? (map :button-released states)))
      (is (every? false? (map :show-context? states)))
      (is (= [[100 200][110 190][119 182][122 175][134 167]]
             (reverse (map (partial take 2) (:trace st9))))))))

