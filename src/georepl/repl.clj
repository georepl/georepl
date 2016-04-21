(ns georepl.repl
  (:require [lanterna.terminal :as terminal]
            [georepl.elements :as elements]
            [clojure.tools.nrepl.server :as nrepl]
            [clojure.tools.nrepl :as repl]))


;; start the nRepl server
(defonce server (nrepl/start-server :port 7888))


(defn- setpos [state target]
  (let [i-max (count (:curline state))
        st (case target
             :null    (assoc state :i (- 0 (count (:prefix state))) :j (inc (:j state)))
             :left    (assoc state :i (max 0 (dec (:i state))))
             :right   (assoc state :i (min (count (:curline state)) (inc (:i state))))
             :home    (assoc state :i 0)
             :end     (assoc state :i (min (count (:curline state)) 10000))
             :next    (assoc state :i 0 :j (inc (:j state)))
                      state)]
    (terminal/move-cursor (:term st) (+ (:i st) (count (:prefix st))) (:j st))
    st))


(defn- output [state s]
  (let [linelen (first (terminal/get-size (:term state)))
        cblk    (repeat linelen \space)
        cout    (concat (:prefix state) s cblk)]
    (terminal/move-cursor (:term state) 0 (:j state))
    (terminal/put-string (:term state) (apply str cout))
    (assoc (setpos state :cur) :curline s)))


(defn- reset [state]
  (assoc state :curline ""
               :done false
               :i 0
               :j (inc (:j state))))


(defn- evaluate [state]
  (try
    (repl/response-values
      (repl/message (:repl state) { :op :eval :code (:curline state) }))
  (catch Exception e
    (prn-str (:cause e)))))


(defn- out-eval [state f-eval]
  (let [s (f-eval state)
        st (setpos state :null)]
    (terminal/put-string (:term st) (prn-str (if (coll? s) (first s) s)))
    (assoc st :curline ""
              :j (inc (:j state))
              :history (vec (dedupe (cons (:curline st) (:history st))))
              :history-index (inc (:history-index st)))))


(defn- home [state]
  (setpos state :home))


(defn- end [state]
  (setpos state :end))


(defn- left [state]
  (setpos state :left))


(defn- right [state]
  (setpos state :right))


(defn- delete [state]
  (if (>= (:i state) (count (:curline state)))
    state
    (let [[s1 s2] (split-at (:i state) (:curline state))
          s (apply str (concat s1 (rest s2)))]
      (output state s))))


(defn- backspace [state]
  (if (<= (:i state) 0)
    state
    (let [[s1 s2] (split-at (dec (:i state)) (:curline state))
          s (apply str (concat s1 (rest s2)))]
      (output (setpos state :left) s))))


(defn- up [state]
  (let [len (max 0 (dec (count (:history state))))
        idx (:history-index state)]
    (if (neg? idx)
      state
      (end
        (assoc
          (output state (nth (:history state) idx)) :history-index (min len (inc idx)))))))


(defn- down [state]
  (let [len (max 0 (dec (count (:history state))))
        idx (:history-index state)]
    (if (neg? idx)
      state
      (end
        (assoc
          (output state (nth (:history state) idx)) :history-index (max 0 (dec idx)))))))


(defn- enter [state]
  (assoc state :done true))


(defn- read-input [state chr]
  (if (and (not= (type chr) clojure.lang.Keyword) (>= (int chr) 32) (<= (int chr) 126))
    (let [[s1 s2] (split-at (:i state) (:curline state))
          s (apply str (concat (conj (vec s1) chr) s2))]
      (right (output state s)))
    state))


(defn- edit-chr [state]
  (let [chr (terminal/get-key (:term state))]
    (case chr
      nil        state
      :home      (home state)
      :end       (end state)
      :left      (left state)
      :right     (right state)
      :delete    (delete state)
      :backspace (backspace state)
      :up        (up state)
      :down      (down state)
      :enter     (enter state)
                 (read-input state chr))))


(defn- on-change [state]
  (if-let [s (elements/curform)]
    (-> (assoc state :curline s :done true)
        (out-eval evaluate)
        (reset)
        (output ""))
    state))


(defn exit [state]
  (terminal/stop (:term state)))


(defn- editor [state]
  (loop [st (output (reset state) "")]
    (if (:exit st)
      (exit st)
      (if (:done st)
        (recur (edit-chr (output (reset (out-eval st evaluate)) "")))
        (recur (edit-chr (on-change st)))))))


; start a lantern terminal
(defn- init []
  (let [term (terminal/get-terminal :swing)]
    (terminal/start term)
    (assoc {} :term term
              :prefix "GeoRepl=> "
              :history []
              :history-index -1
              :j 0)))


;; start nRepl client ...
;; ... and connect to the nRepl server ...
;; ... and return current state
(defn- with-repl [state f]
  (with-open [conn (repl/connect :port 7888)]
    (f (assoc state :repl (repl/client conn 1000)))))


(defn start []
  (with-repl (init) editor))

