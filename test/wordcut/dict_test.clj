(ns wordcut.dict-test
  (:require [clojure.test :refer :all]
            [wordcut.dict :refer :all]))

(defn basic-dict []
  {:content [{:surface "AA"} {:surface "AB"}
             {:surface "BA"} {:surface "BC"}]
   :r 3})

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
  (let [dict (basic-dict)]
    {:s 8 :l 0 :r (:r dict)
     :offset 0 :is-fianl false :dict dict
     :payload nil}))

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
    (let [dict (basic-dict)]
      (is (= (pointers-update () dict "B" 0)
             (list {:s 0 :is-final false
                    :l 2 :r 3 :offset 1
                    :dict dict}))))))

(deftest large-dict-seek-test
  (testing "basic seek"
    (let [dict (read-default-dict "thai")]
      (is (= (dict-seek dict :LEFT
                        0 (:r dict) 0
                        \ข)
             1474))
      (is (= (dict-seek dict :RIGHT
                        0 (:r dict) 0
                        \ข)
             1701))
      (is (= (dict-seek dict :LEFT
                        1474 1701 1 \อ))))))
