(ns wordcut.dict
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn update-index [entries index i]
  (let [entry (nth entries i)
        surface (:surface entry)]
    (loop [index index e-seq (range 1 3)]
      (if (empty? e-seq)
        index
        (let [e (first (e-seq))
              key (subs 0 e surface)]
          (recur (if (get index key)
                   index
                   (assoc! index key i))
                 (rest e-seq)))))))

(defn make-index-left [content]
  (loop [index (transient (hash-map))
         i 0]
    (if (< i (count content))
      (persistent! index)
      (recur (update-index content index i)
             (inc i)))))

(defn make-index-right [content]
  (loop [index (transient (hash-map))
         i (dec (count content))]
    (if (>= i 0)
      (persistent! index)
      (recur (update-index content index i)
             (dec i)))))

(defn read-dict [uri]
  (let [content (into-array (let [lines (str/split-lines (slurp uri))]
                              (map (fn [line] {:surface line})
                                   (sort lines))))]
    {:content content
     :index {:left (make-index-left content)
             :right (make-index-right content)}
     :r (dec (count content))}))

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

(defn dict-seek-bsearch [dict
                         policy
                         ^Integer l
                         ^Integer r
                         ^Integer offset
                         ^Character ch] 
  (let [content (:content dict)]
    (loop [^Integer l l ^Integer r r ^Integer ans nil]
      (if (<= l r)
        (let [m (bit-shift-right (+ l r) 1)
              w (:surface (nth content m))
              wlen (count w)]
          (if (<= wlen offset)
            (recur (+ 1 m) r ans)
            (let [ch-w (nth w offset)
                  cmp (compare ch-w ch)]
              (cond (< cmp 0) (recur (+ 1 m) r ans)
                    (> cmp 0) (recur l (- m 1) ans)
                    (= policy :LEFT) (recur l (- m 1) m)
                    (= policy :RIGHT) (recur (+ 1 m) r m)))))    
        ans))))

(defn dict-seek [dict
                 policy
                 ^Integer l
                 ^Integer r
                 ^Integer offset
                 ^Character ch]
  (cond
    (nil? (get :index dict))
    (dict-seek-bsearch dict policy l r offset ch)
    (= offset 0)
    (let [idx (get (:index dict) policy)
          key (str ch)]
      (get idx key))
    (= offset 1)
    (let [idx (get (:index dict) policy)
          content (:content dict)
          surface (:surface (nth content l))
          key (str (subs surface 0 offset) ch)]
      (get idx key))
    :else
    (dict-seek-bsearch dict policy l r offset ch)))

(defn pointer-update [pointer ch]
  (let [p pointer
        offset (:offset p)
	dict (:dict p)
	l (dict-seek dict :LEFT (:l p) (:r p) offset ch)]
    (when l
      (let [r (dict-seek dict :RIGHT l (:r p) offset ch)
            w (:surface (nth (:content dict) l))
            w-len (count w)]
        {:s (:s p) :l l :r r :offset (inc offset)
         :dict dict
         :is-final (= w-len (inc offset))}))))

(defn pointers-update [pointers dict text i]
  (let [ch (nth text i)]
    (->> (cons {:s i :l 0 :r (:r dict)
                :dict dict :offset 0 :is-final false}
               pointers)
         (map #(pointer-update % ch))
         (remove nil?))))
