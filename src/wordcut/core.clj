(ns wordcut.core
  (:require [wordcut.dict :as d])
  (:require [wordcut.tokenizer :as w])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:use clojopts.core)
  (:gen-class))

(defn -main
  "..."
  [& args]
  (let [opts (clojopts "wordcut"
                       args
                       (with-arg lang l "language"))
        lang (:lang opts)
        dict (cond
               (= lang "khmer") (d/read-default-khmer-dict)
               (= lang "lao") (d/read-default-lao-dict)
               (= lang "thai") (d/read-default-thai-dict)
               :else (d/read-default-thai-dict))
        tokenize (w/tokenizer dict)]
    (doseq [line (line-seq (io/reader *in*))]
      (println (str/join "|" (tokenize line))))))
