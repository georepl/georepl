(defproject georepl "0.1.0-SNAPSHOT"
  :description "Mathematics with Clojure"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [quil "2.2.6"]
;                 [korma "0.4.0"]
;                 [org.xerial/sqlite-jdbc "3.8.11.2"]
                 [lein-light-nrepl "0.2.0"]]
  :profiles {:dev {:plugins [[lein-cloverage "1.0.6"]]}})
