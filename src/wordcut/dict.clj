(ns wordcut.dict
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
  (let [ch (nth text i)
        added-pointers (conj pointers {:s i :l 0 :r (dec (count dict))
                                       :dict dict :offset 0 :is-final false
                                       :payload nil})
        updated-pointers (map #(pointer-update % ch) added-pointers)
        removed-pointers (remove nil? updated-pointers)]
    removed-pointers))
