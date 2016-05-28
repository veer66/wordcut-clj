(ns wordcut.dict
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn read-dict [uri]
  (into-array (let [lines (str/split-lines (slurp uri))]
                (map (fn [line] (vector line))
                     (sort lines)))))

(defn default-thai-dict-url []
  (io/resource "tdict-std.txt"))

(defn read-default-thai-dict []
  (read-dict (default-thai-dict-url)))

(defn default-lao-dict-url []
  (io/resource "laowords.txt"))

(defn read-default-lao-dict []
  (read-dict (default-lao-dict-url)))

(defn default-khmer-dict-url []
  (io/resource "khmerwords.txt"))

(defn read-default-khmer-dict []
  (read-dict (default-khmer-dict-url)))

(defn dict-seek [dict policy l r offset ch]
  (loop [l l r r ans nil]
    (if (<= l r)
      (let [m (bit-shift-right (+ l r) 1)
            w (first (nth dict m))
            wlen (count w)]
        (if (<= wlen offset)
          (recur (+ 1 m) r ans)
          (let [ch-w (nth w offset)
                cmp (compare ch-w ch)]
            (cond (< cmp 0) (recur (+ 1 m) r ans)
                  (> cmp 0) (recur l (- m 1) ans)
                  (= policy :LEFT) (recur l (- m 1) m)
                  (= policy :RIGHT) (recur (+ 1 m) r m)))))    
      ans)))

(defn pointer-update [pointer ch]
  (let [p pointer
        offset (:offset p)
	dict (:dict p)
	l (dict-seek dict :LEFT (:l p) (:r p) offset ch)]
    (when l
      (let [r (dict-seek (:dict p) :RIGHT l (:r p) offset ch)
            w (first (nth dict l))
            w-len (count w)]
        {:s (:s p) :l l :r r :offset (inc offset)
         :dict dict
         :is-final (= w-len (inc offset))}))))

(defn pointers-update [pointers dict text i]
  (let [ch (nth text i)]
    (->> (conj pointers {:s i :l 0 :r (dec (count dict))
                         :dict dict :offset 0 :is-final false})
         (map #(pointer-update % ch))
         (remove nil?))))
