(defproject georepl "0.1.0-SNAPSHOT"
  :description "Mathematics with Clojure"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/tools.nrepl "0.2.11"]
                 [quil "2.4.0"]]
  :profiles {:dev {:plugins [[lein-cloverage "1.0.6"]]}})
