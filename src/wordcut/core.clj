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
        dict (case lang
               "khmer" (d/read-default-khmer-dict)
               "lao" (d/read-default-lao-dict)
               "thai" (d/read-default-thai-dict)
               (d/read-default-thai-dict))
        tokenize (w/tokenizer dict)]
    (doseq [line (line-seq (io/reader *in*))]
      (println (str/join "|" (tokenize line))))))
