(defproject georepl "0.1.0-SNAPSHOT"
  :description "Geometry with Clojure"
  :url "https://github.com/georepl/georepl"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/tools.nrepl "0.2.11"]
                 [quil "2.4.0"]]
  :profiles {:dev {:plugins [[lein-cloverage "1.0.6"]]}}
  :aot [georepl.main]
  :main georepl.main)
