(ns wordcut.dict
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import net.veerkesto.PrefixTree)
  (:import kotlin.Pair))

(defn make-prefix-tree [words]
  (->> words
       (map (fn [word] (Pair. word true)))
       (into-array Pair)
       PrefixTree.))

(defn read-dict [uri]
  (let [words (->> (slurp uri)
                     (str/split-lines)
                     (sort))]
    (make-prefix-tree words)))

(defn read-default-dict [lang]
  (let [lang-files {"khmer" "khmerwords.txt"
                    "lao" "laowords.txt"
                    "thai" "tdict-std.txt"}]
    (read-dict (io/resource (get lang-files lang "thai")))))

;;(.isFinal (.lookup (make-prefix-tree ["A"]) 0 0 \A))

(defn pointer-update [pointer ch]
  (let [p pointer
        offset (:offset p)
        dict (:dict p)
        node-id (:node-id p)
        child (.lookup dict node-id offset ch)]
    (when child
      {:s (:s p)
       :node-id (.getChildId child)
       :offset (inc offset)
       :dict dict
       :is-final (.isFinal child)})))

(defn pointers-update [pointers dict text i]
  (let [ch (nth text i)]
    (->> (cons {:s i
                :node-id 0
                :dict dict
                :offset 0
                :is-final false}
               pointers)
         (map #(pointer-update % ch))
         (remove nil?))))
