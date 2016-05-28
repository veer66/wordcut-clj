(ns wordcut.tokenizer
  (:require [wordcut.dict :refer :all])
  (:gen-class))

(defn better? [o1 o2]
  (reduce (fn [_ attr]
            (let [v1 (get o1 attr)
                  v2 (get o2 attr)]
              (cond
                (< v1 v2) (reduced true)
                (> v1 v2) (reduced false))))
          nil
          '(:unk :chunk)))

(defn best-edge [edges]
  (reduce (fn [best e] (if (better? e best) e best)) edges))

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
        init-dag (transient (vector (take (inc len) (repeat nil))))]
    (persistent!
     (loop [dag (assoc! init-dag 0 {:chunk 0 :unk 0 :s 0 :payload nil :etype :INIT})
            pre-pointers ()
            pre-space-info {:s 0 :is-final false :offset 0}
            i 0
            left 0]
       (if (< i len)
         (let [e (inc i)
               ch (nth text i)
               pointers (pointers-update pre-pointers dict text i)
               final-p (filter (fn [p] (:is-final p))
                               pointers)
               space-info (update-space-info pre-space-info
                                             (is-space ch)
                                             (if (= e len)
                                              false
                                              (is-space (nth text e))))]
           (cond
             (not (empty? final-p))
             (recur (update-dag-by-dict! dag e final-p)
                    pointers space-info e e)
             (:is-final space-info)
             (recur (update-dag-by-space! dag e space-info)
                    pointers space-info e e)
             :else
             (recur (update-dag-by-unk! dag left e)
                    pointers space-info e left)))
         dag)))))
                  
(defn dag-to-list [dag text]
  (loop [e (count text) lst nil]
    (if (= e 0)
      lst
      (let [s (:s (nth dag e))]
        (recur s (cons (subs text s e) lst))))))

(defn tokenizer [dict]
  (fn [text]
    (dag-to-list (build-dag text dict)
                 text)))

(defn lao-tokenizer []
  (tokenizer (read-default-lao-dict)))

(defn khmer-tokenizer []
  (tokenizer (read-default-khmer-dict)))

(defn thai-tokenizer []
  (tokenizer (read-default-thai-dict)))
