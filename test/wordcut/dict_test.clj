(ns wordcut.dict-test
  (:require [clojure.test :refer :all]
            [wordcut.dict :refer :all]))

(defn basic-dict []
  [["AA"] ["AB"] ["BA"] ["BC"]])

(deftest dict-test
  (testing "basic left search"
    (is (= 2 (dict-seek (basic-dict) :LEFT 0 3 0 \B))))
  (testing "basic right search"
    (is (= 3 (dict-seek (basic-dict) :RIGHT 0 3 0 \B))))
  (testing "offset=1 search"
    (is (= 3 (dict-seek (basic-dict) :LEFT 2 3 1 \C)))
    (is (= 1 (dict-seek (basic-dict) :LEFT 0 1 1 \B))))
  (testing "not found"
    (is (nil? (dict-seek (basic-dict) :LEFT 0 3 0 \K)))))

(defn basic-dict-pointer []
  {:s 8 :l 0 :r (dec (count (basic-dict)))
   :offset 0 :is-fianl false :dict (basic-dict)
   :payload nil})

(deftest dict-pointer-test
  (testing "update"
    (let [pointer (pointer-update (basic-dict-pointer) \B)]
      (is (:s pointer) 8)
      (is (:l pointer) 2)
      (is (:r pointer) 3)
      (is (:offset pointer) 1)
      (is (not (:is-final pointer)))))
  (testing "final"
    (is (-> (basic-dict-pointer)
            (pointer-update \A)
            (pointer-update \B)
            :is-final))))


(deftest dict-pointers-test
  (testing "update"
    (let [pointers (pointers-update (basic-dict) [] "B" 0)]
      (is (count pointers) 1)
      (let [pointer (first pointers)]
        (is (:s pointer) 0)
        (is (not (:is-final pointer)))))))
