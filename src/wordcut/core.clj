;;(load "dict")
;;(load "wordcut")

(ns wordcut.core
  (:require [wordcut.dict :as d])
  (:require [wordcut.wordcut :as w])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:gen-class))

(defn tokenizer [dict]
  (fn [text]
    (w/dag-to-list (w/build-dag text dict)
                    text)))



(defn -main
  "..."
  [& args]
  (let [tokenize (tokenizer (d/read-default-thai-dict))]
    (doseq [line (line-seq (io/reader *in*))]
      (println (str/join "|" (tokenize line))))))
