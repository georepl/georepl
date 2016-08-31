(ns georepl.dialog-test
  (:require [clojure.test :refer :all]
            [georepl.mathlib :as math]
            [georepl.dialog :as dialog]))


(defn- foo0 [state]
  (prn "Foo0")
  state)

(defn- foo1 [state]
  (prn "Foo1")
  state)

(defn- foo2 [state]
  (prn "Foo2")
  state)

(defn- foo3 [state]
  (prn "Foo3")
  state)

(defn- cancel [state]
  (assoc state :selection nil))


(def sl [{:s "Zero" :highlight true :f foo0}
         {:s "One" :highlight false :f foo1}
         {:s "Two" :highlight false :f foo2}
         {:s "Three" :highlight false :f foo3}
         {:s "Four" :highlight false :f cancel}])

(def sl2 [{:p1 [341 313], :p2 [413.8369140625 333], :s "Polyline", :f foo0, :type nil, :highlight false}
          {:p1 [341 333], :p2 [413.8369140625 353], :s "Ortho-Polyline", :f foo1, :type nil, :highlight false}
          {:p1 [341 353], :p2 [413.8369140625 373], :s "Point", :f foo2, :type nil, :highlight true}
          {:p1 [341 373], :p2 [413.8369140625 393], :s "Modify Shapes", :f foo3, :type nil, :highlight false}
          {:p1 [341 393], :p2 [413.8369140625 413], :s "Cancel", :f cancel, :type nil, :highlight false}
         ])

(def ret '({:s "Polyline", :f foo0, :type nil, :highlight false}
           {:s "Ortho-Polyline", :f foo1, :type nil, :highlight true}
           {:s "Point", :f foo2, :type nil, :highlight false}
           {:s "Modify Shapes", :f foo3, :type nil, :highlight false}
           {:s "Cancel", :f cancel, :type nil, :highlight false}))

;;; fails when calling quil/test
;(deftest dialog-test
;  (is (= nil (#'dialog/dialog [100 100] sl))))

(deftest in-box-test
  (is (true? (#'dialog/in-box? [110 125][100 100][500 150])))
  (is (false? (#'dialog/in-box? [98 125][100 100][500 150])))
  (is (false? (#'dialog/in-box? [110 155][100 100][500 150])))
  )

;(deftest select-point-test
;  (is (= nil (#'dialog/select-point [100 100] (#'dialog/dialog [100 100] sl)))))

(deftest select-test
  (testing "select key"
    (is (= false (:highlight (nth (vec (#'dialog/select :up sl)) 0))))
    (is (= true (:highlight (nth (vec (#'dialog/select :up sl)) 4))))
    (let [sel (#'dialog/select :up sl2)]
      (is (= (map #(dissoc % :f) ret) (map #(dissoc % :p1 :p2 :f) sel)))
    )))
