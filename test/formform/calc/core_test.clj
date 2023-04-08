(ns formform.calc.core-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [formform.calc.core :refer :all]))

(deftest filter-dna-seq-test
  (testing "Correctness and completeness of selection"
    (are [x y] (= (filter-dna-seq [:M :I :U :N] x) y)
      [0] [:N] [1] [:U] [2] [:I] [3] [:M])
    (are [x y] (= (filter-dna-seq [:N :U :I :M
                                   :U :I :M :I
                                   :I :M :I :U
                                   :M :I :U :N] x) y)
      [0 0] [:N] [0 1] [:U] [0 2] [:I] [0 3] [:M]
      [1 0] [:U] [1 1] [:I] [1 2] [:M] [1 3] [:I]
      [2 0] [:I] [2 1] [:M] [2 2] [:I] [2 3] [:U]
      [3 0] [:M] [3 1] [:I] [3 2] [:U] [3 3] [:N]
      [0 -1] [:M :I :U :N]
      [1 -1] [:I :M :I :U]
      [2 -1] [:U :I :M :I]
      [3 -1] [:N :U :I :M]
      [-1 0] [:M :I :U :N]
      [-1 1] [:I :M :I :U]
      [-1 2] [:U :I :M :I]
      [-1 3] [:N :U :I :M]
      [-1 -1] [:N :U :I :M :U :I :M :I :I :M :I :U :M :I :U :N]))

  (testing "Selection in higher dimensions"
    (are [x y] (= (filter-dna-seq
                   [:N :U :I :M  :U :I :M :I  :I :M :I :U  :M :I :U :N
                    :U :I :M :U  :I :M :I :I  :M :I :U :M  :I :U :N :N
                    :I :M :U :I  :M :I :I :M  :I :U :M :I  :U :N :N :U
                    :M :U :I :M  :I :I :M :I  :U :M :I :U  :N :N :U :I]
                   x) y)
      [3 1 2] [:M]
      [1 0 1] [:N]
      [-1 2 1] [:M :I :I :M]
      [3 -1 2] [:U :I :M :I]
      [0 3 -1] [:M :U :I :M]
      [1 -1 -1] [:I :M :U :I  :M :I :I :M  :I :U :M :I  :U :N :N :U]
      [-1 2 -1] [:U :I :M :I
                 :I :M :I :I
                 :M :I :I :M
                 :I :I :M :I]
      [-1 -1 3] [:N  :U  :I  :M
                 :U  :I  :M  :I
                 :I  :M  :I  :U
                 :M  :I  :U  :N]
      [-1 -1 -1] [:N :U :I :M  :U :I :M :I  :I :M :I :U  :M :I :U :N
                  :U :I :M :U  :I :M :I :I  :M :I :U :M  :I :U :N :N
                  :I :M :U :I  :M :I :I :M  :I :U :M :I  :U :N :N :U
                  :M :U :I :M  :I :I :M :I  :U :M :I :U  :N :N :U :I])

    (are [x y] (= (filter-dna-seq
                   [:N :U :I :M  :U :I :M :I  :I :M :I :U  :M :I :U :N
                    :U :I :M :U  :I :M :I :I  :M :I :U :M  :I :U :N :N
                    :I :M :U :I  :M :I :I :M  :I :U :M :I  :U :N :N :U
                    :M :U :I :M  :I :I :M :I  :U :M :I :U  :N :N :U :I

                    :U :I :M :I  :I :M :I :U  :M :I :U :N  :N :U :I :M
                    :I :M :I :I  :M :I :U :M  :I :U :N :N  :U :I :M :U
                    :M :I :I :M  :I :U :M :I  :U :N :N :U  :I :M :U :I
                    :I :I :M :I  :U :M :I :U  :N :N :U :I  :M :U :I :M

                    :I :M :I :U  :M :I :U :N  :N :U :I :M  :U :I :M :I
                    :M :I :U :M  :I :U :N :N  :U :I :M :U  :I :M :I :I
                    :I :U :M :I  :U :N :N :U  :I :M :U :I  :M :I :I :M
                    :U :M :I :U  :N :N :U :I  :M :U :I :M  :I :I :M :I

                    :M :I :U :N  :N :U :I :M  :U :I :M :I  :I :M :I :U
                    :I :U :N :N  :U :I :M :U  :I :M :I :I  :M :I :U :M
                    :U :N :N :U  :I :M :U :I  :M :I :I :M  :I :U :M :I
                    :N :N :U :I  :M :U :I :M  :I :I :M :I  :U :M :I :U]
                   x) y)

      [2 1 3 0] [:M]
      [1 3 1 2] [:U]
      [-1 1 -1 3] [:I :M :I :U  :M :I :U :I  :I :U :I :M  :U :I :M :I]
      [2 -1 -1 -1] [:U :I :M :I  :I :M :I :U  :M :I :U :N  :N :U :I :M
                    :I :M :I :I  :M :I :U :M  :I :U :N :N  :U :I :M :U
                    :M :I :I :M  :I :U :M :I  :U :N :N :U  :I :M :U :I
                    :I :I :M :I  :U :M :I :U  :N :N :U :I  :M :U :I :M])))


(deftest consts->quaternary-test
  (testing "Correctness of conversion"
    (is (= "4r0123" (consts->quaternary [:N :U :I :M])))
    (is (= "4r1232232232232232" (consts->quaternary [:U :I :M :I
                                                     :I :M :I :I
                                                     :M :I :I :M
                                                     :I :I :M :I])))
    (is (= "4r0312" (consts->quaternary [:N :M :U :I])))
    ;; should this return nil?
    (is (= "4r23" (consts->quaternary [:I :M])))
    (is (= "4r0" (consts->quaternary [:N])))
    (is (thrown? AssertionError
                 (consts->quaternary [:_])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (consts->quaternary [])))))


(deftest permute-dna-seq-test
  (testing "Correctness of permutation"
    (is (= (permute-dna-seq
            {}
            [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N] [1 0])
           [[1 0] [:M :I :U :N :M :I :U :N :M :I :U :N :M :I :U :N]]))))


(deftest dna-seq-perspectives-test
  (testing "Correctness of permutations"
    (is (= (dna-seq-perspectives
            {}
            [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N])
           [[[0 1] [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N]]
            [[1 0] [:M :I :U :N :M :I :U :N :M :I :U :N :M :I :U :N]]]))))

