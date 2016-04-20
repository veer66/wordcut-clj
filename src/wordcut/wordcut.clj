(ns wordcut.wordcut
  (:require [wordcut.dict :refer :all])
  (:require [clojure.pprint :as pprint])
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
         (let [s (:s p)
               src (nth dag s)]
           {:etype :DICT
            :s s
            :unk (:unk src)
            :chunk (inc (:chunk src))
            :payload (:payload src)}))
       pointers))

(defn update-dag-by-dict! [dag i final-pointers]
  (let [best (best-edge (build-dict-edges dag final-pointers))]
    (assoc! dag i best)))

(defn build-unk-edge [dag left]
  (let [src (nth dag left)]
    {:s left
     :unk (inc (:unk src))
     :chunk (inc (:chunk src))
     :payload nil
     :etype :UNK}))

(defn update-space-info [space-info current-is-space next-is-space]
  (cond
    (:is-final space-info) {:s (+ (:s space-info)
                                  (:offset space-info)
                                  1)
                            :offset 0
                            :is-final false}
    current-is-space {:s (:s space-info)
                      :offset (inc (:offset space-info))
                      :is-final (not next-is-space)}
    :else {:s (+ (:s space-info)
                 (:offset space-info)
                 1)
           :offset 0
           :is-final false}))
  
(defn update-dag-by-space! [dag i space-info]
  (let [s (:s space-info)
        src (nth dag s)]
    (assoc! dag i {:chunk (inc (:chunk src))
                   :unk (:unk src)
                   :s s
                   :payload nil
                   :etype :SPACE})))

(defn update-dag-by-unk! [dag left i]
  (assoc! dag i (build-unk-edge dag left)))

(defn is-space [ch]
  (cond
    (= ch \space) true
    (= ch \t) true
    (= ch \r) true
    (= ch \n) true
    :else false))

(defn build-dag [text dict]
  (let [len (count text)
        dag (transient (vector (take (inc len) (repeat nil))))]
    (assoc! dag 0 {:chunk 0 :unk 0 :s 0 :payload nil :etype :INIT})
    (loop [pre-pointers []
           pre-space-info {:s 0 :is-final false :offset 0}
           i 0
           left 0]
      (when (< i len)
        (let [e (inc i)
              ch (nth text i)
              pointers (pointers-update pre-pointers dict text i)
              final-p (filter (fn [p] (:is-final p)) pointers)
              space-info (update-space-info pre-space-info
                                            (is-space ch)
                                            (if (= e len)
                                              false
                                              (is-space (nth text e))))]
;;          (pprint/pprint pointers)
          (cond
            (> (count final-p) 0)
            (do
              (update-dag-by-dict! dag e final-p)
              (recur pointers space-info e e))
            (:is-final space-info)
            (do
              (update-dag-by-space! dag e space-info)
              (recur pointers space-info e e))
            :else (do
                    (update-dag-by-unk! dag left e)
                    (recur pointers space-info e left)))
          )))
    (persistent! dag)))
                  
(defn dag-to-list [dag text]
  (loop [e (count text) lst nil]
    (if (= e 0)
      lst
      (let [s (:s (nth dag e))]
        (recur s (cons (subs text s e) lst))))))
