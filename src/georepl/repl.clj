(ns georepl.repl
  (:require [lanterna.terminal :as terminal]
            [clojure.tools.nrepl.server :as nrepl]
            [clojure.tools.nrepl :as repl]
            [georepl.elements :as elements]))


;; start the nRepl server
(defonce server (nrepl/start-server :port 7888))


(defn- output [state s]
  (let [linelen (max (count s) (count (:curline state)))
        sblank  (apply str (repeat (max 0 (- linelen (count s))) \space))]
    (terminal/move-cursor (:term state) 0 (:j state))
    (terminal/put-string (:term state) (:prefix state))
    (terminal/move-cursor (:term state) (count (:prefix state)) (:j state))
    (terminal/put-string (:term state) (apply str (concat s sblank)))
    (assoc state :curline s)))


(defn- setpos [state i j]
  (let [i-new (max
                0
                (min
                  (count (:curline state)) i))]
    (terminal/move-cursor (:term state) (+ i-new (count (:prefix state))) j)
    (assoc state :i i-new :j j)))


(defn- reset [state]
  (output (assoc state :curline ""
                       :done false
                       :i (count (:prefix state))
                       :j (inc (:j state)))
          ""))


(defn- evaluate [state]
  (terminal/move-cursor (:term state) 0 (inc (:j state)))

  (try
    (let [s (repl/response-values
              (repl/message (:repl state) {:op :eval :code (:curline state)}))]

      (terminal/put-string (:term state) (prn-str (if (coll? s) (first s) s)))
      (assoc (setpos state 0 (inc (:j state))) :curline ""
                                               :history (vec (dedupe (cons (:curline state) (:history state))))
                                               :history-index 0))
  (catch Exception e
    (terminal/put-string (:term state) (prn-str (:cause e)))
    (assoc (setpos state 0 (inc (:j state))) :curline ""))))



(defn- edit-chr [state]
  (let [chr (terminal/get-key-blocking (:term state))]
    (case chr
      :home      (setpos state 0 (:j state))

      :end       (setpos state 10000 (:j state))

      :left      (setpos state (dec (:i state)) (:j state))

      :right     (setpos state (inc (:i state)) (:j state))

      :delete    (if (>= (:i state) (count (:curline state)))
                   state
                   (let [[s1 s2] (split-at (:i state) (:curline state))
                         s (apply str (concat s1 (rest s2)))]
                     (setpos (output state s) (:i state) (:j state))))

      :backspace (if (<= (:i state) 0)
                   state
                   (let [i-new (dec (:i state))
                         [s1 s2] (split-at i-new (:curline state))
                         s (apply str (concat s1 (rest s2)))]
                     (setpos (output state s) i-new (:j state))))

      :up        (let [idx (:history-index state)
                       len (max 0 (dec (count (:history state))))]
                    (assoc (output state (nth (:history state) idx)) :history-index (min len (inc idx))))

      :down      (let [idx (:history-index state)]
                    (assoc (output state (nth (:history state) idx)) :history-index (max 0 (dec idx))))

      :enter     (assoc state :done true)


                 (if (and (not= (type chr) clojure.lang.Keyword) (>= (int chr) 32) (<= (int chr) 126))
                   (let [[s1 s2] (split-at (:i state) (:curline state))
                         s (apply str (concat (conj (vec s1) chr) s2))]
                     (setpos (output state s) (inc (:i state)) (:j state)))
                   state))))


(defn- editor [state]
  (loop [st (reset state)]
    (if (:done st)
      (recur (edit-chr (reset (evaluate st))))
      (recur (edit-chr st)))))


(defn init []
  (let [term (terminal/get-terminal :swing)]

    ;; start a lantern terminal
    (terminal/start term)

    ;; start nRepl server ...
    ;; ... and connect to the nRepl server ...
    ;; ... and return current state
    (with-open [conn (repl/connect :port 7888)]
      (editor (assoc {} :term term
                        :repl (repl/client conn 1000)
                        :prefix "GeoRepl=> "
                        :history []
                        :j 0)))))

