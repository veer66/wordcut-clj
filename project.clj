(defproject wordcut "1.0"
  :description "Word breaker for ASEAN languages written in Clojure"
  :url "https://github.com/veer66/wordcut-clj"
  :license {:name "LGPL v3"
            :url "http://www.gnu.org/licenses/lgpl.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clojopts "0.3.5"]
                 [net.veerkesto/prefixtree "1.0"]]
  :main ^:skip-aot wordcut.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
