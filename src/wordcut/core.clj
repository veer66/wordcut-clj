(ns wordcut.core
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pprint])
  (:gen-class))

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
 
(defn build-path-infos-basic [path edge_pointers i]
  (filter (fn [p] (not (nil? p)))
          (map (fn [pointer]
                 (let [_p  (- (- i
                                 (pointer :str_offset))
                              1)]
                   (if (< _p 0)
                     (struct path-info nil 1 0 :dict)
                     (let [p _p
                           _path_info (nth path p)]
                       (if (nil? _path_info)
                         nil
                         (let [w (+ 1 (_path_info :w))
                               unk (_path_info :unk)]
                           (struct path-info p w unk :dict)))))))
               edge_pointers)))

(defn build-path-info-unk [path left_boundary]
  (if (nil? left_boundary)
    (struct path-info left_boundary 1 1 :unk)
    (let [_path_info (nth path left_boundary)
          _w (_path_info :w)
          _unk (_path_info :unk)]
      (struct path-info left_boundary (+ _w 1) (+ _unk 1) :unk))))

(defn build-path-infos [path edge_pointers i left_boundary]
  (let [basic_path_infos (build-path-infos-basic path edge_pointers i)]
    (if (> (count basic_path_infos) 0)
      basic_path_infos
      [(build-path-info-unk path left_boundary)])))

(defn select-path [paths]
  (reduce (fn [selected_path path]
            (if (nil? selected_path)
              path
              (if (or (< (path :unk) (selected_path :unk))
                      (and (= (path :unk) (selected_path :unk))
                           (< (path :w) (selected_path :w))))
                path
                selected_path)))
          nil
          paths))

(defn match-rules? [text left_boundary i]
  (let [left_boundary_ (if (nil? left_boundary)
                         0
                         (+ left_boundary 1))
        text_ (apply str
                     (drop left_boundary_
                           (take (+ 1 i) text)))]
    (not (nil? (re-matches #"^[A-Za-z0-9]+$" text_)))))

(defn build-path-info-by-rules [path left_boundary i]
  (if (nil? left_boundary)
    (struct path-info nil 1 0 :rule)
    (let [_path_info (nth path left_boundary)
          w (_path_info :w)
          unk (_path_info :unk)]
      (struct path-info left_boundary (+ w 1) unk :rule))))

(defn build-path [dict text]
  (let [len (count text)
        move-pointer (create-move-pointer dict)]
    (loop [i 0
           pointers [(create-pointer dict)]
           path []
           left_boundary nil]
      (if (= len i)
        path
        (let [ch (nth text i)
              move-pointer_ (fn [pointer] (move-pointer pointer ch))
              not-nil? (fn [pointer] (not (nil? pointer)))
              pointers_  (filter not-nil?
                                 (map move-pointer_ pointers))
              boundary_? (fn [pointer] (boundary? dict pointer))
              edge_pointers  (filter boundary_? pointers_)
              possible_path_infos (build-path-infos path
                                                    edge_pointers
                                                    i
                                                    left_boundary)
              _selected_path (select-path possible_path_infos)
              selected_path (if (and (= :unk (_selected_path :type))
                                     (match-rules? text left_boundary i))
                              (build-path-info-by-rules path left_boundary i)
                              _selected_path)
              path_ path]
          (do
            (recur (+ i 1)
                   (conj pointers_ (create-pointer dict))
                   (conj path_ selected_path)
                   (if (= :dict (selected_path :type))
                     i
                     left_boundary))))))))

(defstruct word-range :s :e)

(defn path-to-ranges [path]
  (let [len (count path)]
    (loop [i (- len 1) ranges (list)]
      (if (nil? i)
        ranges
        (let [info (nth path i)
              p (info :p)
              p_ (if (nil? p) 0 (+ 1 p))
              ]
          (recur p
                 (cons (struct word-range
                               p_ (+ 1 i))
                       ranges)))))))

(defn ranges-to-text [ranges text]
  (let [texts (map (fn [r] (apply str
                                 (drop (r :s)
                                       (take (r :e)
                                            text))))
                   ranges)]
    (str/join "|" texts)))
(defn -main
  "..."
  [& args]
  (let [data (slurp "data1.txt")
        dict (read-dict "tdict-std.txt")
        path (build-path dict data)
        ranges (path-to-ranges path)
        o (ranges-to-text ranges data)]
    (println o)))
