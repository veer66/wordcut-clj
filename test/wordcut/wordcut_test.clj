(ns wordcut.dict-test
  (:require [clojure.test :refer :all]
            [wordcut.wordcut :refer :all]))

(deftest better-test
  (testing "has less unk"
    (is (better? {:unk 10 :chunk 20}
                 {:unk 20 :chunk 1})))
  (testing "has less chunk"
    (is (better? {:unk 10 :chunk 10}
                 {:unk 10 :chunk 20}))
    (is (not (better? {:unk 10 :chunk 20}
                      {:unk 10 :chunk 10})))))

(deftest best-test
  (is (:id (best-edge [{:unk 5 :chunk 10}
                       {:unk 1 :chunk 20 :id :best}
                       {:unk 3 :chunk 3}]))
      :best))


(defn basic-dag []
  [{:etype :INIT :s 0 :unk 0 :chunk 0 :payload \A}
   {:etype :UNK  :s 0 :unk 1 :chunk 1 :payload \B}
   {:etype :DICT :s 0 :unk 0 :chunk 1 :payload \C}])

(defn basic-pointers []
  [{:s 0 :is-final true}
   {:s 1 :is-final true}
   {:s 2 :is-final true}])



(deftest build-dict-edges-test
  (testing "simple"
    (is (= (count (build-dict-edges (basic-dag) (basic-pointers))) 3))
    (is (= (:unk (nth (build-dict-edges (basic-dag) (basic-pointers)) 1)) 1))
    (is (= (:chunk (nth (build-dict-edges (basic-dag) (basic-pointers)) 1)) 2))
    (is (= (:payload (nth (build-dict-edges (basic-dag) (basic-pointers)) 2)) \C))
    (is (= (:etype (nth (build-dict-edges (basic-dag) (basic-pointers)) 1)) :DICT))))

(deftest update-dag-by-dict-test
  (let [dag (transient (conj (basic-dag) nil))
        final-pointers (basic-pointers)]
    (update-dag-by-dict! dag 3 final-pointers)
    (is (= (:s (nth dag 3)) 0))
    (is (= (:chunk (nth dag 3)) 1))))
    

      
;;(deftest update-dag-dict-test
;;  (is (update-dag-dict ((basic-dag) 
