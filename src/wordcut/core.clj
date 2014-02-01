(ns wordcut.core
  (:require [clojure.string :as str])
  (:gen-class))

;(defn remove-empty-line [lines]
;(filter (fn [line] (not= line ""))
;        lines))


(defstruct dict-pointer :str_offset :l :r)

(defn create-dict-iter [dict str_offset ch pos]  
  (fn [_l _r]
    (loop [l _l r _r ans nil]
      (if (<= l r)
        (let [m (unchecked-divide-int (+ l r) 2)
              dict_item (nth dict m)
              len (count dict_item)]
          (if (< len str_offset)
            (recur (+ m 1) r ans)
            (let [_ch (nth dict_item str_offset)
                  cmp (compare _ch ch)]
              (if (< cmp 0)
                (recur (+ m 1) r ans)
                (if (> cmp 0)
                  (recur l (- m 1) ans)
                  (if (= pos :first)
                    (recur l (- m 1) m)
                    (recur (+ m 1) r m)))))))
        ans))))

(defn next-str-offset [offset]
  (if (nil? offset)
    0
    (+ 1 offset)))

(defn create-pointer [dict]
  (struct dict-pointer
          nil
          0
          (- (count dict)
             1)))

(defn create-move-pointer [dict]
  (let [dict_len (count dict)]
    (fn [pointer ch]
      (let [str_offset (pointer :str_offset)
            _str_offset (next-str-offset str_offset)
            dict-iter-left (create-dict-iter dict _str_offset ch :first)
            _l (pointer :l)
            _r (pointer :r)
            l (dict-iter-left _l _r)]
        
        (if (nil? l)
          nil
          (let [dict-iter-right (create-dict-iter dict _str_offset ch :right)
                r (dict-iter-right l _r)]
            (struct dict-pointer _str_offset l r)))))))

(defn read-dict [path]
  (let [lines (str/split-lines (slurp path))]
    lines))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
