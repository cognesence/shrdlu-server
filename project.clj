(defproject shrdlu-server "0.1.0-SNAPSHOT"
  :description "The server side of the SHRDLU example used at ELS 2015 and Game On."
  :url "https://github.com/cognesence/shrdlu-server"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojars.cognesence/matcher "1.0.1"]
                 [org.clojars.cognesence/ops-search "1.0.1"]]
  :main ^:skip-aot org.clojars.cognesence.shrdlu-server.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
