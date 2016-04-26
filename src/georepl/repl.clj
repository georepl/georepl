(ns georepl.repl
  (:require [georepl.shapes-factory :as shapesFactory]
            [georepl.elements :as elements]
            [clojure.tools.nrepl.server :as repl]
            [georepl.configuration :as config]))


;; load all shapes into the current namespace so they are conveniently accessible from a client
;;
(defn- update-elements [s]
  (if (seq? s)
    (doseq [s s]
      (load-string s))
    (load-string s)))


(defn start []
  [(repl/start-server
    :port (:repl-server-port config/Configuration))
   update-elements])


(defn stop [server]
  (repl/stop-server server))


;; 'public' interface for user interaction via repl
;;
(defn point [p]
  (shapesFactory/->PointFactory {:p p}))


(defn line [p q]
  (shapesFactory/->PointFactory {:p1 p :p2 q}))


;;Synopsis: (message client {:keys [id], :as msg, :or {id (uuid)}})
;;Bsp.:     (message (client (connect :port 7888) 1000)  { :op :eval :code (:curline state) })
;;
;;(client-session client & {:keys [session clone]})
;;
;;Synopsis: (client transport response-timeout)
;;Bsp.: (client (connect :port 7888) 1000)
;;
;;
;;(client transport response-timeout)
;;(connect & {:keys [port host transport-fn], :or {transport-fn bencode, host localhost}})
;;
;;(response-values (message (:repl state) { :op :eval :code (:curline state) }))
;;(response-values responses)

