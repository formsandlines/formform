(ns formform.calc.core-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [formform.calc.specs :refer [fns-with-specs]]
            [formform.calc.core :refer :all]
            [orchestra.spec.test :as stest]))

(doseq [fsym fns-with-specs] (stest/instrument fsym))

(def rv (comp vec reverse))


(deftest filter-dna-seq-test
  (testing "Correctness and completeness of selection"
    (are [x y] (= (filter-dna-seq [:N :U :I :M] x) y)
      [0] [:N] [1] [:U] [2] [:I] [3] [:M])
    (are [x y] (= (filter-dna-seq [:N :U :I :M
                                   :U :I :M :I
                                   :I :M :I :U
                                   :M :I :U :N] x) y)
      [0 0] [:N] [0 1] [:U] [0 2] [:I] [0 3] [:M]
      [1 0] [:U] [1 1] [:I] [1 2] [:M] [1 3] [:I]
      [2 0] [:I] [2 1] [:M] [2 2] [:I] [2 3] [:U]
      [3 0] [:M] [3 1] [:I] [3 2] [:U] [3 3] [:N]
      [0 -1] [:N :U :I :M]
      [1 -1] [:U :I :M :I]
      [2 -1] [:I :M :I :U]
      [3 -1] [:M :I :U :N]
      [-1 0] [:N :U :I :M]
      [-1 1] [:U :I :M :I]
      [-1 2] [:I :M :I :U]
      [-1 3] [:M :I :U :N]
      [-1 -1] [:N :U :I :M :U :I :M :I :I :M :I :U :M :I :U :N]))

  (testing "Selection in higher dimensions"
    (are [x y] (= (filter-dna-seq
                   [:I :U :N :N  :U :I :M :U  :I :M :I :I  :M :I :U :M
                    :U :N :N :U  :I :M :U :I  :M :I :I :M  :I :U :M :I
                    :N :N :U :I  :M :U :I :M  :I :I :M :I  :U :M :I :U
                    :N :U :I :M  :U :I :M :I  :I :M :I :U  :M :I :U :N]
                   x) y)
      [3 1 2] [:M]
      [1 0 1] [:N]
      [-1 2 1] [:M :I :I :M]
      [3 -1 2] [:I :M :I :U]
      [0 3 -1] [:M :I :U :M]
      [1 -1 -1] [:U :N :N :U :I :M :U :I :M :I :I :M :I :U :M :I]
      [-1 2 -1] [:I :M :I :I
                 :M :I :I :M
                 :I :I :M :I
                 :I :M :I :U]
      [-1 -1 3] [:N :U :I :M
                 :U :I :M :I
                 :I :M :I :U
                 :M :I :U :N]
      [-1 -1 -1] [:I :U :N :N  :U :I :M :U  :I :M :I :I  :M :I :U :M
                  :U :N :N :U  :I :M :U :I  :M :I :I :M  :I :U :M :I
                  :N :N :U :I  :M :U :I :M  :I :I :M :I  :U :M :I :U
                  :N :U :I :M  :U :I :M :I  :I :M :I :U  :M :I :U :N])

    (are [x y] (= (filter-dna-seq
                   [:U :I :M :U  :I :M :I :I  :M :I :U :M  :I :U :N :N
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
                    :N :N :U :I  :M :U :I :M  :I :I :M :I  :U :M :I :U
                    :N :U :I :M  :U :I :M :I  :I :M :I :U  :M :I :U :N]
                   x) y)

      [2 1 3 0] [:M]
      [1 3 1 2] [:U]
      [-1 1 -1 3] [:I :M :I :U  :M :I :U :I  :I :U :I :M  :U :I :M :I]
      [2 -1 -1 -1] [:M :I :U :M  :I :U :N :N  :U :I :M :U  :I :M :I :I
                    :I :U :M :I  :U :N :N :U  :I :M :U :I  :M :I :I :M
                    :U :M :I :U  :N :N :U :I  :M :U :I :M  :I :I :M :I
                    :M :I :U :N  :N :U :I :M  :U :I :M :I  :I :M :I :U])))


(deftest consts->quaternary-test
  (testing "Correctness of conversion"
    (is (= "0123" (consts->quaternary [:N :U :I :M])))
    (is (= "1232232232232232" (consts->quaternary [:U :I :M :I
                                                   :I :M :I :I
                                                   :M :I :I :M
                                                   :I :I :M :I])))
    (is (= "0312" (consts->quaternary [:N :M :U :I])))
    ;; should this return nil?
    (is (= "23" (consts->quaternary [:I :M])))
    (is (= "0" (consts->quaternary [:N])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (consts->quaternary [:_])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (consts->quaternary [])))))


(deftest permute-dna-seq-test
  (testing "Correctness of permutation"
    (is (= (permute-dna-seq
            {}
            [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N] [1 0])
           [[1 0] [:M :I :U :N :M :I :U :N :M :I :U :N :M :I :U :N]])))
  (testing "Correctness of a complete set of permutations"
    (let [perms
          [;; a b c
           [[0 1 2] [:N :N :N :N :N :N :N :N :N :N :N :N :N :N :N :N :U :U :U :U :N :U :N :U :U :U :U :U :N :U :N :U :I :I :I :I :I :I :I :I :N :N :I :I :N :N :I :I :M :M :M :M :I :M :I :M :U :U :M :M :N :U :I :M]]
           ;; a c b
           [[0 2 1] [:N :N :N :N :N :N :N :N :N :N :N :N :N :N :N :N :U :N :U :N :U :U :U :U :U :N :U :N :U :U :U :U :I :I :N :N :I :I :N :N :I :I :I :I :I :I :I :I :M :I :U :N :M :M :U :U :M :I :M :I :M :M :M :M]]
           ;; b a c
           [[1 0 2] [:N :N :N :N :U :U :U :U :I :I :I :I :M :M :M :M :N :N :N :N :N :U :N :U :I :I :I :I :I :M :I :M :N :N :N :N :U :U :U :U :N :N :I :I :U :U :M :M :N :N :N :N :N :U :N :U :N :N :I :I :N :U :I :M]]
           ;; b c a
           [[1 2 0] [:N :U :I :M :N :U :I :M :N :U :I :M :N :U :I :M :N :N :I :I :N :U :I :M :N :N :I :I :N :U :I :M :N :U :N :U :N :U :N :U :N :U :I :M :N :U :I :M :N :N :N :N :N :U :N :U :N :N :I :I :N :U :I :M]]
           ;; c a b
           [[2 0 1] [:N :N :N :N :U :N :U :N :I :I :N :N :M :I :U :N :N :N :N :N :U :U :U :U :I :I :N :N :M :M :U :U :N :N :N :N :U :N :U :N :I :I :I :I :M :I :M :I :N :N :N :N :U :U :U :U :I :I :I :I :M :M :M :M]]
           ;; c b a
           [[2 1 0] [:N :U :I :M :N :N :I :I :N :U :N :U :N :N :N :N :N :U :I :M :N :U :I :M :N :U :N :U :N :U :N :U :N :U :I :M :N :N :I :I :N :U :I :M :N :N :I :I :N :U :I :M :N :U :I :M :N :U :I :M :N :U :I :M]]]]
      (doseq
          [[porder dna :as p] perms]
        (is (= (permute-dna-seq {} (second (first perms)) porder)
               p))))))


(deftest dna-seq-perspectives-test
  (testing "Correctness of permutations"
    (is (= (dna-seq-perspectives
            {}
            [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N])
           [[[0 1] [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N]]
            [[1 0] [:M :I :U :N :M :I :U :N :M :I :U :N :M :I :U :N]]])))
  (testing "Expected permutation properties"
    (let [psps (dna-seq-perspectives ;; (a (b (c))) , a → b → c
                {}
                [:N :N :N :N :N :N :N :N :N :N :N :N :N :N :N :N
                 :U :U :U :U :N :U :N :U :U :U :U :U :N :U :N :U
                 :I :I :I :I :I :I :I :I :N :N :I :I :N :N :I :I
                 :M :M :M :M :I :M :I :M :U :U :M :M :N :U :I :M])]
      ;; expected value ordering
      (is (= (map first psps)
             [[0 1 2] [0 2 1] [1 0 2] [1 2 0] [2 0 1] [2 1 0]]))
      ;; every permutation should have the same value frequencies
      (is (= #{{:N 25, :U 15, :I 15, :M 9}}
             (into #{} (map (comp frequencies second) psps)))))))

