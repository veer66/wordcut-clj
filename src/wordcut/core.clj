(ns wordcut.core
  (:require [wordcut.dict :as d]
            [wordcut.tokenizer :as w]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:use clojopts.core)
  (:gen-class))

(defn -main
  "..."
  [& args]
  (let [opts (clojopts "wordcut"
                       args
                       (with-arg lang l "language"))
        lang (get opts :lang "thai")
        dict (d/read-default-dict lang)
        tokenize (w/tokenizer dict)]
    (doseq [line (line-seq (io/reader *in*))]
      (println (str/join "|" (tokenize line))))))
