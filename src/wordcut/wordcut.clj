(ns wordcut.wordcut
  (:require [wordcut.dict :refer :all])
  (:gen-class))

(defn better? [o1 o2]
  (loop [attrs '(:unk :chunk) ans nil]
    (if (nil? attrs)
      ans
      (let [attr (first attrs)
            v1 (get o1 attr)
            v2 (get o2 attr)]
        (cond
          (< v1 v2) (recur nil true)
          (> v1 v2) (recur nil false)
          :else (recur (rest attrs) nil))))))

(defn best-edge [edges]
  (reduce (fn [best e]
            (cond
              (nil? best) e
              (better? e best) e
              :else best))
          nil edges))

(defn build-dict-edges [dag pointers]
  (map (fn [p]
         (let [src (nth dag (:s p))]
           {:etype :DICT
            :s (:s src)
            :unk (:unk src)
            :chunk (inc (:chunk src))
            :payload (:payload src)}))
       pointers))

(defn update-dag-by-dict! [dag i final-pointers]
  (let [best (best-edge (build-dict-edges dag final-pointers))]
    (assoc! dag i best)))
