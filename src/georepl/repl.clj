(ns georepl.repl
  (:require [lanterna.terminal :as terminal]
            [georepl.elements :as elements]
            [clojure.tools.nrepl.server :as nrepl]
            [clojure.tools.nrepl :as repl]))


;; start the nRepl server
(defonce server (nrepl/start-server :port 7888))


(defn- output
  ([state s]
(prn "Immediate")
    (let [linelen (first (terminal/get-size (:term state)))
          cblk    (repeat linelen \space)
          cout    (concat (:prefix state) s cblk)]
      (terminal/move-cursor (:term state) 0 (:j state))
      (terminal/put-string (:term state) (apply str cout))
      (assoc state :curline s)))
  ([state]
(prn "Indirect")
    (output state "")))


(defn- setpos [state i j]
  (let [i-new (max
                0
                (min
                  (count (:curline state)) i))]
    (terminal/move-cursor (:term state) (+ i-new (count (:prefix state))) j)
    (assoc state :i i-new :j j)))


(defn- reset [state]
  (assoc state :curline ""
               :done false
               :i (count (:prefix state))
               :j (inc (:j state))))


(defn- next-line [state]
  (terminal/move-cursor (:term state) 0 (inc (:j state))))


(defn- evaluate [state]
  (next-line state)

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
  (let [chr (terminal/get-key (:term state))]
    (case chr
      nil        state

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

      :up        (let [len (max 0 (dec (count (:history state))))
                       idx (:history-index state)]
(prn "UP, len:" len "idx:" idx)
                   (setpos
                     (assoc
                       (output state (nth (:history state) idx)) :history-index (min len (inc idx)))
                         10000 (:j state)))

      :down      (let [len (max 0 (dec (count (:history state))))
                       idx (:history-index state)]
(prn "DOWN, len:" len "idx:" idx)
                   (setpos
                     (assoc
                       (output state (nth (:history state) idx)) :history-index (max 0 (dec idx)))
                         10000 (:j state)))

      :enter     (assoc state :done true)

                 (if (and (not= (type chr) clojure.lang.Keyword) (>= (int chr) 32) (<= (int chr) 126))
                   (let [[s1 s2] (split-at (:i state) (:curline state))
                         s (apply str (concat (conj (vec s1) chr) s2))]
                     (setpos (output state s) (inc (:i state)) (:j state)))
                   state))))


(defn- on-change [state]
  (if-let [s (elements/curform)]
    (-> (assoc state :curline s :done true)
        (evaluate)
        (reset)
        (output))
    state))


(defn- editor [state]
  (loop [st (output (reset state))]
;(prn "STATE:" (dissoc st :term :repl :prefix :history))
    (if (:exit st)
      (exit st)
      (if (:done st)
        (recur (edit-chr (output (reset (evaluate st)))))
        (recur (edit-chr (on-change st)))))))


; start a lantern terminal
(defn- init []
  (let [term (terminal/get-terminal :swing)]
    (terminal/start term)
    (assoc {} :term term
              :prefix "GeoRepl=> "
              :history []
              :j 0)))


;; start nRepl server ...
;; ... and connect to the nRepl server ...
;; ... and return current state
(defn- with-repl [state f]
  (with-open [conn (repl/connect :port 7888)]
    (f (assoc state :repl (repl/client conn 1000)))))


(defn start []
  (with-repl (init) editor))


(defn exit [state]
  (terminal/stop (:term state)))

