(defproject wordcut "0.1.0-SNAPSHOT"
  :description "Thai word breaker in Clojure"
  :url "http://example.com/FIXME"
  :license {:name "LGPL v3"
            :url "http://www.gnu.org/licenses/lgpl.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :main ^:skip-aot wordcut.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
