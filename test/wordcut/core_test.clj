(ns wordcut.core-test
  (:require [clojure.test :refer :all]
            [wordcut.core :refer :all]))

(deftest dict-pointer-test
  (let [dict (read-dict "test/test_dict.txt")
        pointer (create-pointer dict)
        move-pointer (create-move-pointer dict)]
    (testing "basic move"
      (is (= (struct dict-pointer 20 0 1)
             (struct dict-pointer 20 0 1)))
      (is (not= (struct dict-pointer 20 0 0)
                (struct dict-pointer 21 0 0)))
      (is (= pointer (struct dict-pointer nil 0 8)))
      (is (= (move-pointer pointer (nth "ก" 0))
             (struct dict-pointer 0 0 1)))
      (is (nil? (move-pointer pointer (nth "X" 0)))))))

(deftest dict-reader-test
  (let [dict (read-dict "test/test_dict.txt")]
    (testing "read-dict"
      (is (= (second dict) "กกต"))
      (is (= (count dict) 9)))))
