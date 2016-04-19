(defproject wordcut "0.1.0-SNAPSHOT"
  :description "Word breaker for ASEAN languages written in Clojure"
  :url "http://example.com/FIXME"
  :license {:name "LGPL v3"
            :url "http://www.gnu.org/licenses/lgpl.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot wordcut.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
