(ns formform.calc.core-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [formform.calc.specs :refer [fns-with-specs]]
            [formform.calc.core :refer :all]
            [formform.utils :as utils]
            [orchestra.spec.test :as stest]))

(doseq [fsym fns-with-specs] (stest/instrument fsym))


(defn weight-sample-approximation
  [n weights]
  (->> (utils/rng-select-n (utils/make-rng)
                           [\a \b \c \d]
                           n
                           weights)
       (frequencies)
       (sort-by key)
       (mapv (fn [[k v]] [k (Math/round (/ v (/ (float n) 10)))]))))

(deftest random-weights-test
  (testing "weight argument normalization"
    (is (= (conform-nuim-weights {:m 3 :u 2}) [0 2 0 3]))
    (is (= (conform-nuim-weights 0.2) [0.8 0.2 0.2 0.2])))
  (testing "weights approximation with sufficient sample size"
    (is (= (weight-sample-approximation 1000 [1 2 3 4])
           [[\a 1] [\b 2] [\c 3] [\d 4]]))))

(deftest filter-dna-seq-test
  (testing "Correctness and completeness of selection"
    (are [x y] (= (filter-dna-seq [:n :u :i :m] x) y)
      [0] [:n] [1] [:u] [2] [:i] [3] [:m])
    (are [x y] (= (filter-dna-seq [:n :u :i :m
                                   :u :i :m :i
                                   :i :m :i :u
                                   :m :i :u :n] x) y)
      [0 0] [:n] [0 1] [:u] [0 2] [:i] [0 3] [:m]
      [1 0] [:u] [1 1] [:i] [1 2] [:m] [1 3] [:i]
      [2 0] [:i] [2 1] [:m] [2 2] [:i] [2 3] [:u]
      [3 0] [:m] [3 1] [:i] [3 2] [:u] [3 3] [:n]
      [0 -1] [:n :u :i :m]
      [1 -1] [:u :i :m :i]
      [2 -1] [:i :m :i :u]
      [3 -1] [:m :i :u :n]
      [-1 0] [:n :u :i :m]
      [-1 1] [:u :i :m :i]
      [-1 2] [:i :m :i :u]
      [-1 3] [:m :i :u :n]
      [-1 -1] [:n :u :i :m :u :i :m :i :i :m :i :u :m :i :u :n]))

  (testing "Selection in higher dimensions"
    (are [x y] (= (filter-dna-seq
                   [:i :u :n :n  :u :i :m :u  :i :m :i :i  :m :i :u :m
                    :u :n :n :u  :i :m :u :i  :m :i :i :m  :i :u :m :i
                    :n :n :u :i  :m :u :i :m  :i :i :m :i  :u :m :i :u
                    :n :u :i :m  :u :i :m :i  :i :m :i :u  :m :i :u :n]
                   x) y)
      [3 1 2] [:m]
      [1 0 1] [:n]
      [-1 2 1] [:m :i :i :m]
      [3 -1 2] [:i :m :i :u]
      [0 3 -1] [:m :i :u :m]
      [1 -1 -1] [:u :n :n :u :i :m :u :i :m :i :i :m :i :u :m :i]
      [-1 2 -1] [:i :m :i :i
                 :m :i :i :m
                 :i :i :m :i
                 :i :m :i :u]
      [-1 -1 3] [:n :u :i :m
                 :u :i :m :i
                 :i :m :i :u
                 :m :i :u :n]
      [-1 -1 -1] [:i :u :n :n  :u :i :m :u  :i :m :i :i  :m :i :u :m
                  :u :n :n :u  :i :m :u :i  :m :i :i :m  :i :u :m :i
                  :n :n :u :i  :m :u :i :m  :i :i :m :i  :u :m :i :u
                  :n :u :i :m  :u :i :m :i  :i :m :i :u  :m :i :u :n])

    (are [x y] (= (filter-dna-seq
                   [:u :i :m :u  :i :m :i :i  :m :i :u :m  :i :u :n :n
                    :i :m :u :i  :m :i :i :m  :i :u :m :i  :u :n :n :u
                    :m :u :i :m  :i :i :m :i  :u :m :i :u  :n :n :u :i
                    :u :i :m :i  :i :m :i :u  :m :i :u :n  :n :u :i :m

                    :i :m :i :i  :m :i :u :m  :i :u :n :n  :u :i :m :u
                    :m :i :i :m  :i :u :m :i  :u :n :n :u  :i :m :u :i
                    :i :i :m :i  :u :m :i :u  :n :n :u :i  :m :u :i :m
                    :i :m :i :u  :m :i :u :n  :n :u :i :m  :u :i :m :i

                    :m :i :u :m  :i :u :n :n  :u :i :m :u  :i :m :i :i
                    :i :u :m :i  :u :n :n :u  :i :m :u :i  :m :i :i :m
                    :u :m :i :u  :n :n :u :i  :m :u :i :m  :i :i :m :i
                    :m :i :u :n  :n :u :i :m  :u :i :m :i  :i :m :i :u

                    :i :u :n :n  :u :i :m :u  :i :m :i :i  :m :i :u :m
                    :u :n :n :u  :i :m :u :i  :m :i :i :m  :i :u :m :i
                    :n :n :u :i  :m :u :i :m  :i :i :m :i  :u :m :i :u
                    :n :u :i :m  :u :i :m :i  :i :m :i :u  :m :i :u :n]
                   x) y)

      [2 1 3 0] [:m]
      [1 3 1 2] [:u]
      [-1 1 -1 3] [:i :m :i :u  :m :i :u :i  :i :u :i :m  :u :i :m :i]
      [2 -1 -1 -1] [:m :i :u :m  :i :u :n :n  :u :i :m :u  :i :m :i :i
                    :i :u :m :i  :u :n :n :u  :i :m :u :i  :m :i :i :m
                    :u :m :i :u  :n :n :u :i  :m :u :i :m  :i :i :m :i
                    :m :i :u :n  :n :u :i :m  :u :i :m :i  :i :m :i :u])))


(deftest consts->quaternary-test
  (testing "Correctness of conversion"
    (is (= "0123" (consts->quaternary [:n :u :i :m])))
    (is (= "1232232232232232" (consts->quaternary [:u :i :m :i
                                                   :i :m :i :i
                                                   :m :i :i :m
                                                   :i :i :m :i])))
    (is (= "0312" (consts->quaternary [:n :m :u :i])))
    ;; should this return nil?
    (is (= "23" (consts->quaternary [:i :m])))
    (is (= "0" (consts->quaternary [:n])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (consts->quaternary [:_])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (consts->quaternary [])))))


(deftest permute-dna-seq-test
  (testing "Correctness of permutation"
    (is (= (permute-dna-seq
            {}
            [:m :m :m :m :i :i :i :i :u :u :u :u :n :n :n :n] [1 0])
           [[1 0] [:m :i :u :n :m :i :u :n :m :i :u :n :m :i :u :n]])))
  (testing "Correctness of a complete set of permutations"
    (let [perms
          [;; a b c
           [[0 1 2] [:n :n :n :n :n :n :n :n :n :n :n :n :n :n :n :n :u :u :u :u :n :u :n :u :u :u :u :u :n :u :n :u :i :i :i :i :i :i :i :i :n :n :i :i :n :n :i :i :m :m :m :m :i :m :i :m :u :u :m :m :n :u :i :m]]
           ;; a c b
           [[0 2 1] [:n :n :n :n :n :n :n :n :n :n :n :n :n :n :n :n :u :n :u :n :u :u :u :u :u :n :u :n :u :u :u :u :i :i :n :n :i :i :n :n :i :i :i :i :i :i :i :i :m :i :u :n :m :m :u :u :m :i :m :i :m :m :m :m]]
           ;; b a c
           [[1 0 2] [:n :n :n :n :u :u :u :u :i :i :i :i :m :m :m :m :n :n :n :n :n :u :n :u :i :i :i :i :i :m :i :m :n :n :n :n :u :u :u :u :n :n :i :i :u :u :m :m :n :n :n :n :n :u :n :u :n :n :i :i :n :u :i :m]]
           ;; b c a
           [[1 2 0] [:n :u :i :m :n :u :i :m :n :u :i :m :n :u :i :m :n :n :i :i :n :u :i :m :n :n :i :i :n :u :i :m :n :u :n :u :n :u :n :u :n :u :i :m :n :u :i :m :n :n :n :n :n :u :n :u :n :n :i :i :n :u :i :m]]
           ;; c a b
           [[2 0 1] [:n :n :n :n :u :n :u :n :i :i :n :n :m :i :u :n :n :n :n :n :u :u :u :u :i :i :n :n :m :m :u :u :n :n :n :n :u :n :u :n :i :i :i :i :m :i :m :i :n :n :n :n :u :u :u :u :i :i :i :i :m :m :m :m]]
           ;; c b a
           [[2 1 0] [:n :u :i :m :n :n :i :i :n :u :n :u :n :n :n :n :n :u :i :m :n :u :i :m :n :u :n :u :n :u :n :u :n :u :i :m :n :n :i :i :n :u :i :m :n :n :i :i :n :u :i :m :n :u :i :m :n :u :i :m :n :u :i :m]]]]
      (doseq
          [[porder dna :as p] perms]
        (is (= (permute-dna-seq {} (second (first perms)) porder)
               p))))))


(deftest dna-seq-perspectives-test
  (testing "Correctness of permutations"
    (is (= (dna-seq-perspectives
            {}
            [:m :m :m :m :i :i :i :i :u :u :u :u :n :n :n :n])
           [[[0 1] [:m :m :m :m :i :i :i :i :u :u :u :u :n :n :n :n]]
            [[1 0] [:m :i :u :n :m :i :u :n :m :i :u :n :m :i :u :n]]])))
  (testing "Expected permutation properties"
    (let [psps (dna-seq-perspectives ;; (a (b (c))) , a → b → c
                {}
                [:n :n :n :n :n :n :n :n :n :n :n :n :n :n :n :n
                 :u :u :u :u :n :u :n :u :u :u :u :u :n :u :n :u
                 :i :i :i :i :i :i :i :i :n :n :i :i :n :n :i :i
                 :m :m :m :m :i :m :i :m :u :u :m :m :n :u :i :m])]
      ;; expected value ordering
      (is (= (map first psps)
             [[0 1 2] [0 2 1] [1 0 2] [1 2 0] [2 0 1] [2 1 0]]))
      ;; every permutation should have the same value frequencies
      (is (= #{{:n 25, :u 15, :i 15, :m 9}}
             (into #{} (map (comp frequencies second) psps)))))))

