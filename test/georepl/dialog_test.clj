(ns georepl.dialog-test
  (:require [clojure.test :refer :all]
            [georepl.mathlib :as math]
            [georepl.draw-framework :as fw]
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

(deftest dialog-test
  (#'fw/init-renderer :test)
  (is (nil? (#'dialog/dialog [100 100] nil)))
  (is (nil? (#'dialog/dialog [100 100] [])))
  (let [sel (#'dialog/dialog [100 100] sl)]
    (is (= [5 100] (#'math/vec-sub (:p2 (last sel))(:p1 (first sel)))))
    ))

(deftest in-box-test
  (is (true? (#'dialog/in-box? [110 125][100 100][500 150])))
  (is (false? (#'dialog/in-box? [98 125][100 100][500 150])))
  (is (false? (#'dialog/in-box? [110 155][100 100][500 150])))
  )

(deftest select-point-test
  (#'fw/init-renderer :test)
  (testing "edge-cases"
    (#'dialog/select-point nil nil)
    (#'dialog/select-point [] [])
    (#'dialog/select-point nil (#'dialog/dialog [100 100] sl))
    (#'dialog/select-point [] (#'dialog/dialog [100 100] sl))
    (#'dialog/select-point [100 100] nil)
    (#'dialog/select-point [100 100] []))
  (testing "dialog starts at p, clicked on p"
    (let [sel (#'dialog/select-point [100 100] (#'dialog/dialog [100 100] sl))]
      (is (= "Zero" (:s sel)))))
  (testing "dialog starts at p, clicked on point outside"
    (let [sel (#'dialog/select-point [90 90] (#'dialog/dialog [100 100] sl))]
      (is (nil? sel))))
  (testing "dialog starts at p, clicked on point inside, select nth dialog element"
    (let [sel (#'dialog/select-point [102 130] (#'dialog/dialog [100 100] sl))]
      (is (= "One" (:s sel))))))

(deftest up-down-selection
  (#'fw/init-renderer :test)
  (let [sel (#'dialog/dialog [300 300] sl)]
    (is (= "Zero" (:s (#'dialog/current-selection sel))))
    (is (= "One" (:s (#'dialog/current-selection (#'dialog/down sel)))))
    (is (= "Three" (:s (#'dialog/current-selection (#'dialog/down (#'dialog/down (#'dialog/down sel)))))))
    (is (= "Four" (:s (#'dialog/current-selection (#'dialog/up sel)))))
    (is (= "Four" (:s (#'dialog/current-selection (#'dialog/up (#'dialog/down (#'dialog/up sel)))))))
    ))

(deftest pre-select-test
  (testing "edge-cases"
    (is (empty? (#'dialog/select :up nil)))
    (is (empty? (#'dialog/select :up [])))
    (is (empty? (#'dialog/select :down nil)))
    (is (empty? (#'dialog/select :down [])))
    (is (empty? (#'dialog/select :ok nil)))
    (is (empty? (#'dialog/select :ok [])))
    )
  (testing "select key with :up"
    (is (false? (:highlight (nth (vec (#'dialog/select :up sl)) 0))))
    (is (true? (:highlight (nth (vec (#'dialog/select :up sl)) 4))))
    (let [sel (#'dialog/select :up sl2)]
      (is (= (map #(dissoc % :f) ret) (map #(dissoc % :p1 :p2 :f) sel)))
    ))
  (testing "select key with :down"
    (is (false? (:highlight (nth (vec (#'dialog/select :down sl)) 0))))
    (is (true? (:highlight (nth (vec (#'dialog/select :down sl)) 1))))
    (let [sel (#'dialog/select :down sl2)]
      (is (= [false false false true false] (map :highlight sel)))
    ))
  (testing "select key with :ok"
    (is (true? (:highlight (nth (vec (#'dialog/select :ok sl)) 0))))
    (is (false? (:highlight (nth (vec (#'dialog/select :ok sl)) 1))))
    (let [sel (#'dialog/select :ok sl2)]
      (is (= [false false true false false] (map :highlight sel)))
    ))
  (testing "select key with any other key"
    (is (true? (:highlight (nth (vec (#'dialog/select :foo sl)) 0))))
    (is (false? (:highlight (nth (vec (#'dialog/select :foo sl)) 1))))
    (let [sel (#'dialog/select :foo sl2)]
      (is (= [false false true false false] (map :highlight sel)))
    ))
  (testing "select with point"
    (is (nil? (#'dialog/select 102 130 nil)))
    (is (nil? (#'dialog/select 102 130 [])))
    (is (true? (:highlight (nth (vec (#'dialog/select 102 130 sl)) 0))))
    (is (false? (:highlight (nth (vec (#'dialog/select 102 100 sl)) 1))))
    )
  )
