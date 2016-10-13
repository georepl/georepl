(ns georepl.draw-framework-test
  (:require [clojure.test :refer :all]
            [georepl.shapes :as shapes]
            [georepl.elements :as elements]
            [georepl.draw-framework :as fw]))


(deftest setup-test
  (#'fw/init-renderer :test)
  (testing "setup-gui"
    (let [state (#'fw/setup-gui)]
      (is (= georepl.gui.Drawing (type state)))
      (testing "draw-gui-test"
        (is (=  {:show-trace? false, :text-visible? true, :mouse-moved? false, :button-released 0, :show-pointlist? false, :show-context? false, :attrbs {:show-pointlist? false, :show-context? false, :text-visible? true}}
                (dissoc (#'fw/draw-gui state) :redo-stack :selection :context-menu)))))))

(def redo {:drw-elem #georepl.shapes.Compound {:elems [] :type :compound :subtype :drawing :visible 0 :p-ref nil :filename "/testfiles/temp.grl"} :shapes-list () :points-list ()})

(deftest key-pressed-gui-test
  (#'fw/init-renderer :test)
  (#'elements/push-elem (#'shapes/constructCompound [] :subtype :drawing :filename "/testfiles/temp.grl"))
    (let [state (#'fw/setup-gui)]
      (is (= state (#'fw/key-pressed-gui state {:key-code 10 :key 1})))
      (is (= state (#'fw/key-pressed-gui state {:key-code 27 :key 1})))
      (is (= state (#'fw/key-pressed-gui state {:key-code 82 :key 1})))
      (is (= state (#'fw/key-pressed-gui state {:key-code 83 :key 1})))
      (is (= (assoc state :redo-stack [redo]) (#'fw/key-pressed-gui state {:key-code 90 :key 1})))
      (is (= state (#'fw/key-pressed-gui state {:key-code 42 :key 1})))
      ))

(deftest key-pressed-gallery-test
  (#'fw/init-renderer :test)
  (#'elements/clear)
  (#'elements/push-elem (#'shapes/constructCompound [] :subtype :drawing :filename "/testfiles/temp.grl"))
    (let [state (#'fw/setup-gui)
          new-state (assoc state :f-on-close #(prn %))]
      (is (= state (dissoc (#'fw/key-pressed-gallery new-state {:key-code 10 :key 1}) :f-on-close :complete)))
      (is (= state (#'fw/key-pressed-gallery state {:key-code 27 :key 1})))
      (is (= state (#'fw/key-pressed-gallery state {:key-code 82 :key 1})))
      (is (= state (#'fw/key-pressed-gallery state {:key-code 83 :key 1})))
      (is (= state (#'fw/key-pressed-gallery state {:key-code 90 :key 1})))
      (is (= state (#'fw/key-pressed-gallery state {:key-code 42 :key 1})))
      ))
