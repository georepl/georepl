(ns georepl.repl-test
  (:require [clojure.test :refer :all]
            [georepl.repl :as repl]))



(deftest nrepl-test
  (testing "evaluate"
    (let [st1 (assoc (#'repl/reset (#'repl/init)) :curline "(reduce + (interleave [10 100 1000] (range)))")]
      (is (= [1113] (#'repl/with-repl st1 #'repl/evaluate)))
      (repl/exit st1))))


(deftest lanterna-test
  (let [st (#'repl/init)]
    (testing "init"
      (is (= (dissoc st :term) {:prefix "GeoRepl=> " :history-index -1 :history [] :j 0}))
      (is (not (nil? (:term st)))))
    (testing "setpos and reset"
      (let [st1 (assoc (#'repl/reset st) :curline "When shall we three meet again?")]
        (let [st1_1 (#'repl/setpos st1 :null)]
          (is (= -10 (:i st1_1)))
          (is (= 2 (:j st1_1)))
          (let [st1_2 (#'repl/setpos st1_1 :null)]
            (is (= -10 (:i st1_2)))
            (is (= 3 (:j st1_2)))
            (let [st1_3 (#'repl/setpos st1_2 :left)]
              (is (= 0 (:i st1_3)))
              (is (= 3 (:j st1_3)))
              (let [st1_4 (#'repl/setpos st1_3 :right)]
                (is (= 1 (:i st1_4)))
                (is (= 3 (:j st1_4)))
                (let [st1_5 (#'repl/setpos st1_4 :home)]
                  (is (= 0 (:i st1_5)))
                  (is (= 3 (:j st1_5)))
                  (let [st1_6 (#'repl/setpos st1_5 :end)]
                    (is (= 31 (:i st1_6)))
                    (is (= 3 (:j st1_6)))
                    (let [st1_7 (#'repl/setpos st1_6 :next)]
                      (is (= 0 (:i st1_7)))
                      (is (= 4 (:j st1_7))))))))))))

    (testing "output"
      (let [st1 (#'repl/reset st)]
        (let [st1_1 (#'repl/output st1 "In thunder, lightning, or in rain")]
          (is (= "In thunder, lightning, or in rain" (:curline st1_1)))
          (is (= 0 (:i st1_1)))
          (is (= 1 (:j st1_1)))
          )))

    (testing "editor primitives"
      (let [st1 (assoc (#'repl/reset st) :curline "When the hurlyburly's done. When the battle's lost and won")]
        (let [st1_1 (#'repl/home st1)]
          (is (= 0 (:i st1_1)))
          (is (= 1 (:j st1_1)))
          (let [st1_2 (#'repl/left st1_1)]
            (is (= "When the hurlyburly's done. When the battle's lost and won" (:curline st1_2)))
            (is (= 0 (:i st1_2)))
            (is (= 1 (:j st1_2)))
            (let [st1_3 (#'repl/right (#'repl/right (#'repl/right st1_2)))]
              (is (= "When the hurlyburly's done. When the battle's lost and won" (:curline st1_3)))
              (is (= 3 (:i st1_3)))
              (is (= 1 (:j st1_3)))
              (let [st1_4 (#'repl/delete st1_3)]
                (is (= "Whe the hurlyburly's done. When the battle's lost and won" (:curline st1_4)))
                (is (= 3 (:i st1_4)))
                (is (= 1 (:j st1_4)))
                (let [st1_5 (#'repl/end st1_4)]
                  (is (= "Whe the hurlyburly's done. When the battle's lost and won" (:curline st1_5)))
                  (is (= 57 (:i st1_5)))
                  (is (= 1 (:j st1_5)))
                  (let [st1_6 (#'repl/backspace (#'repl/backspace st1_5))]
                    (is (= "Whe the hurlyburly's done. When the battle's lost and w" (:curline st1_6)))
                    (is (= 55 (:i st1_6)))
                    (is (= 1 (:j st1_6)))
                    (let [st1_7 (#'repl/home st1_6)]
                      (is (= "Whe the hurlyburly's done. When the battle's lost and w" (:curline st1_7)))
                      (is (= 0 (:i st1_7)))
                      (is (= 1 (:j st1_7)))
                      (let [st1_8 (-> st1_7
                                      (assoc :curline "That will be 'ere the set of sun.")
                                      (#'repl/enter)
                                      (assoc :curline "Where the place? - Upon the heath.")
                                      (#'repl/enter))]
                        (is (= "Where the place? - Upon the heath." (:curline st1_8)))
                        (is (= 0 (:i st1_8)))
                        (is (= 1 (:j st1_8)))
                        (let [st1_9 (#'repl/up st1_8)]
                          (is (= "Where the place? - Upon the heath." (:curline st1_9)))
                          (is (= 0 (:i st1_9)))
                          (is (= 1 (:j st1_9)))
                          (let [st1_10 (#'repl/down st1_9)]
                            (is (= "Where the place? - Upon the heath." (:curline st1_10)))
                            (is (= 0 (:i st1_10)))
                            (is (= 1 (:j st1_10)))
                            ))))))))))))

    (repl/exit st)))
