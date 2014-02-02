(ns wordcut.core
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pprint])
  (:gen-class))

;(defn remove-empty-line [lines]
;(filter (fn [line] (not= line ""))
;        lines))


(defstruct dict-pointer :str_offset :l :r)

(defn boundary? [dict pointer]
  (and (not (nil? pointer))
       (not (nil? (pointer :str_offset)))
       (= (count (nth dict
                      (pointer :l)))
          (+ 1 (pointer :str_offset)))))

(defn create-dict-iter [dict str_offset ch pos]  
  (fn [_l _r]
    (loop [l _l r _r ans nil]
      (if (<= l r)
        (let [m (unchecked-divide-int (+ l r) 2)
              dict_item (nth dict m)
              len (count dict_item)]
          (if (<= len str_offset)
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

(defstruct path-info :p :w :unk :type)

(defn build-path-infos [path edge_pointers i]
  (map (fn [pointer]
         (let [_p  (- (- i
                        (pointer :str_offset))
                      1)
               p (if (< _p 0) nil _p)
               w (if (nil? p)
                   1
                   (+ ((nth path p) :w) 1))
               unk 0 type :dict]
           (do 
             (struct path-info p w unk type))))
       edge_pointers))

(defn select-path [paths]
  (if (> (count paths) 0)
    (apply max-key
           (fn [path] (path :w))
           paths)
    nil))

(defn build-path [dict text]
  (let [len (count text)
        move-pointer (create-move-pointer dict)]
    (loop [i 0
           pointers [(create-pointer dict)]
           path []]
      (if (= len i)
        path
        (let [ch (nth text i)
              move-pointer_ (fn [pointer] (move-pointer pointer ch))
              not-nil? (fn [pointer] (not (nil? pointer)))
              pointers_  (filter not-nil?
                                 (map move-pointer_ pointers))
              boundary_? (fn [pointer] (boundary? dict pointer))
              edge_pointers (filter boundary_? pointers_)
              possible_path_infos (build-path-infos path edge_pointers i)
              selected_path (select-path possible_path_infos)]
          (recur (+ i 1)
                 (conj pointers_ (create-pointer dict))
                 (conj path selected_path)))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
