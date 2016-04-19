(ns wordcut.dict
  (:require [clojure.string :as str])
  (:gen-class))

(defn dict-seek [dict policy l r offset ch]
  (loop [l l r r ans nil]
    (if (<= l r)
      (let [m (int (/ (+ l r) 2))
            w (first (nth dict m))
            wlen (count w)]
        (if (<= wlen offset)
          (recur (+ 1 m) r ans)
          (let [ch-w (nth w offset)
                cmp (compare ch-w ch)]
            (println cmp)
            (cond (< cmp 0) (recur (+ 1 m) r ans)
                  (> cmp 0) (recur l (- m 1) ans)
                  (= policy :LEFT) (recur l (- m 1) m)
                  (= policy :RIGHT) (recur (+ 1 m) r m)))))    
      ans)))
