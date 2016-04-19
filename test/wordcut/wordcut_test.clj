(ns wordcut.dict-test
  (:require [clojure.test :refer :all]
            [wordcut.wordcut :refer :all]))OA

(defn basic-dict []
  [["AA"] ["AB"] ["BA"] ["BC"]])

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

(deftest build-unk-edge-test
  (testing "basic"
    (is (= (:s (build-unk-edge (basic-dag) 1)) 1))
    (is (= (:chunk (build-unk-edge (basic-dag) 1)) 2))
    (is (= (:unk (build-unk-edge (basic-dag) 1)) 2))
    (is (nil? (:payload (build-unk-edge (basic-dag) 1))))
    (is (= (:etype (build-unk-edge (basic-dag) 1)) :UNK))))

(deftest update-space-info-test
  (testing "non-final to final"
    (is (not (:is-final (update-space-info {:s 0 :offset 0 :is-final false}
                                           false false))))
    (is (not (:is-final (update-space-info {:s 0 :offset 0 :is-final false}
                                           true true))))
    (is (not (:is-final (update-space-info {:s 0 :offset 0 :is-final false}
                                           false true))))
    (is (:is-final (update-space-info {:s 0 :offset 0 :is-final false}
                                      true false))))
  (testing "offset"
    (is (= (-> {:s 10 :offset 0 :is-final false}
               (update-space-info false false)
               (update-space-info false false)
               (update-space-info false false))
           {:offset 0 :s 13 :is-final false}))
  (testing "s"
    (is (= (:s (update-space-info {:s 11 :offset 0 :is-final false}
                              false
                              false))
           12)))
  (testing "from final"
    (is (= (update-space-info {:s 20 :offset 30 :is-final true}
                              false
                              false)
           {:s 51 :offset 0 :is-final false}))
    (is (= (update-space-info {:s 20 :offset 30 :is-final true}
                              true
                              false)
           {:s 51 :offset 0 :is-final false})))))

(deftest update-dag-by-space-test
  (testing "basic"
    (let [dag (transient (basic-dag))]
      (update-dag-by-space! dag 3 {:s 2 :offset 1 :is-final true})
      (is (= (nth dag 3)
             {:chunk 2 :unk 0 :payload nil :s 2 :etype :SPACE})))))

(deftest build-dag-test
  (testing "basic"
    (is (= (build-dag "" (basic-dict))
           [{:chunk 0 :unk 0 :payload nil :etype :INIT :s 0}]))
    (is (= (build-dag "X" (basic-dict))
           [{:chunk 0 :unk 0 :payload nil :etype :INIT :s 0}
            {:chunk 1 :unk 1 :payload nil :etype :UNK :s 0}]
           ))
    (is (= (build-dag "AB" (basic-dict))
           [{:chunk 0 :unk 0 :payload nil :etype :INIT :s 0}
            {:chunk 1 :unk 1 :payload nil :etype :UNK :s 0}
            {:chunk 1 :unk 0 :payload nil :etype :DICT :s 0}]
           ))
    (is (= (nth (build-dag "ABX" (basic-dict)) 3)
           {:chunk 2 :unk 1 :payload nil :etype :UNK :s 2}
           ))
    (is (= (nth (build-dag "AB X" (basic-dict)) 3)
           {:chunk 2 :unk 0 :payload nil :etype :SPACE :s 2}
           ))
    (is (= (nth (build-dag "AB X" (basic-dict)) 4)
           {:chunk 3 :unk 1 :payload nil :etype :UNK :s 3}))))
  
(deftest dag-to-vec-test
  (testing "basic"
    (is (= (dag-to-list [{:chunk 0 :unk 0 :payload nil :etype :INIT :s 0}
                         {:chunk 1 :unk 1 :payload nil :etype :UNK :s 0}
                         {:chunk 1 :unk 0 :payload nil :etype :DICT :s 0}
                         {:chunk 2 :unk 0 :payload nil :etype :SPACE :s 2}
                         {:chunk 3 :unk 1 :payload nil :etype :UNK :s 3}]
                        "AB X")
           ["AB" " " "X"]))))
