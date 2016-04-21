(defproject wordcut "0.1.1"
  :description "Word breaker for ASEAN languages written in Clojure"
  :url "https://github.com/veer66/cl-wordcut"
  :license {:name "LGPL v3"
            :url "http://www.gnu.org/licenses/lgpl.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clojopts "0.3.5"]]
  :main ^:skip-aot wordcut.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
