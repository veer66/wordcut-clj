;;(load "dict")
;;(load "wordcut")

(ns wordcut.core
  (:require [wordcut.dict :as d])
  (:require [wordcut.wordcut :as w])
  (:gen-class))

(defn tokenizer [dict]
  (fn [text]
    (w/dag-to-list (w/build-dag text dict)
                    text)))

(defn -main
  "..."
  [& args]
  (let [tokenize (tokenizer (d/read-default-thai-dict))]
    (println (tokenize "ขอขอขอ"))))

;;((tokenizer (d/read-dict (d/default-thai-dict-url))) "กากากา")
