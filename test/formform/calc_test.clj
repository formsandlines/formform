(ns formform.calc-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [formform.calc :as calc :refer :all]
            [formform.specs.calc]
            [clojure.spec.test.alpha :as stest]))

(stest/instrument 'formform.calc/digit->const)
(stest/instrument 'formform.calc/char->const)
(stest/instrument 'formform.calc/const->digit)

(stest/instrument 'formform.calc/dna-dimension)
(stest/instrument 'formform.calc/rand-dna)
(stest/instrument 'formform.calc/dna->quaternary)
(stest/instrument 'formform.calc/make-compare-dna)
(stest/instrument 'formform.calc/reorder-dna-seq)

(stest/instrument 'formform.calc/prod=dna-seq->dna)
(stest/instrument 'formform.calc/prod=dna->dna-seq)
(stest/instrument 'formform.calc/digits->dna)
(stest/instrument 'formform.calc/chars->dna)
(stest/instrument 'formform.calc/dna->digits)
(stest/instrument 'formform.calc/expand-dna-seq)
(stest/instrument 'formform.calc/reduce-dna-seq)
(stest/instrument 'formform.calc/make-dna)
(stest/instrument 'formform.calc/filter-dna-seq)
(stest/instrument 'formform.calc/filter-dna)

(stest/instrument 'formform.calc/permute-dna-seq)
(stest/instrument 'formform.calc/dna-seq-perspectives)
(stest/instrument 'formform.calc/dna-perspectives)
(stest/instrument 'formform.calc/equal-dna)
(stest/instrument 'formform.calc/equiv-dna)

(stest/instrument 'formform.calc/rand-vpoint)
(stest/instrument 'formform.calc/vspace)
(stest/instrument 'formform.calc/vdict)
(stest/instrument 'formform.calc/dna->vdict)
(stest/instrument 'formform.calc/vdict->vmap)
(stest/instrument 'formform.calc/dna->vmap)

(stest/instrument 'formform.calc/rel)
(stest/instrument 'formform.calc/inv)


;; ? should test dna functions with lists too

(deftest dna-dimension-test
  (testing "Dimension for sequences of 4^n elements"
    (is (== (dna-dimension [0]) 0))
    (is (== (dna-dimension '(0)) 0))
    (is (== (dna-dimension (range 4)) 1))
    (is (== (dna-dimension (range 16)) 2))
    (is (== (dna-dimension (range 64)) 3))
    (is (== (dna-dimension (range 256)) 4)))

  (testing "Incorrect number of elements"
    (is (nil? (dna-dimension '())))
    (is (nil? (dna-dimension (range 2))))
    (is (nil? (dna-dimension (range 8))))))


(deftest dna?-test
  (testing "Content validity"
    (is (dna? [:N :U :U :M]))
    (is (not (dna? :NUUM)))
    (is (not (dna? [:_])))
    (is (not (dna? [:N :A :I :M])))
    (is (not (dna? [0 1 2 3])))
    (is (not (dna? ["N"]))))

  (testing "Dimension validity"
    (is (dna? '(:N)))
    (is (dna? [:U :N :I :U
               :I :U :N :U
               :U :U :N :U
               :N :N :U :U]))
    (is (not (dna? '(:M :N))))
    (is (not (dna? '())))
    (is (not (dna? '(:N :U :I))))))


(deftest rand-dna-test
  (testing "dna validity"
    (doseq [n (range 6)]
      (is (dna? (rand-dna n))))
    (is (dna-dimension? (rand-dna 4 [0 1 2])))
    (is (dna-dimension? (rand-dna 6 [nil])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (rand-dna 4 [:x :y :z :v :w])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (rand-dna 1 [])))
    ))


(deftest make-dna-test
  (testing "Correct transformation of mixed element types"
    (is (= (make-dna :M \U \I 0 [1 2 1 3] :U :U :I \n [1 2 1 3])
           [:M :U :I :N :U :I :U :M :U :U :I :N :U :I :U :M])
        (= (make-dna \n \1 :U \M
                     :I :U \n \M
                     [:M :U 2 :N]
                     [\n \n \u 3])
           [:N :U :U :M :I :U :N :M :M :U :I :N :N :N :U :M])))
  (testing "Correct dna-dimension for nested dna-seqs"
    (is (= (dna-dimension
            (make-dna (rand-dna 2) (rand-dna 2) (rand-dna 2) (rand-dna 2)))
           3))
    (is (= (dna-dimension
            (make-dna [[nuim-code nuim-code nuim-code nuim-code]
                       [nuim-code nuim-code nuim-code nuim-code]
                       [nuim-code nuim-code nuim-code nuim-code]
                       [nuim-code nuim-code nuim-code nuim-code]]
                      [[nuim-code nuim-code nuim-code nuim-code]
                       [nuim-code nuim-code nuim-code nuim-code]
                       [nuim-code nuim-code nuim-code nuim-code]
                       [nuim-code nuim-code nuim-code nuim-code]]
                      [[nuim-code nuim-code nuim-code nuim-code]
                       [nuim-code nuim-code nuim-code nuim-code]
                       [nuim-code nuim-code nuim-code nuim-code]
                       [nuim-code nuim-code nuim-code nuim-code]]
                      [[nuim-code nuim-code nuim-code nuim-code]
                       [nuim-code nuim-code nuim-code nuim-code]
                       [nuim-code nuim-code nuim-code nuim-code]
                       [nuim-code nuim-code nuim-code nuim-code]]))
           4)))) 


(deftest dna->digits-test
  (testing "Correctness of conversion"
    (is (= (dna->digits [:N :U :I :M]) '(0 1 2 3)))
    (is (= (dna->digits [:N :U :I :M] nmui-code) '(2 3 0 1)))
    (is (= (dna->digits
            '(:M :M :M :M  :I :I :I :I  :U :U :U :U  :N :N :N :N
                 :I :M :I :M  :M :M :M :M  :N :U :N :U  :U :U :U :U
                 :U :U :M :M  :N :N :I :I  :M :M :M :M  :I :I :I :I
                 :N :U :I :M  :U :U :M :M  :I :M :I :M  :M :M :M :M)
            nmui-code)
           [1 1 1 1  2 0 0 2  1 3 3 1  2 2 2 2
            0 3 0 3  1 1 1 1  2 1 2 1  3 3 3 3
            3 3 3 3  2 2 2 2  1 1 1 1  0 0 0 0
            2 1 2 1  1 3 3 1  2 3 0 1  1 1 1 1]))))


(deftest digits->dna-test
  (testing "Correctness of conversion"
    (is (= (digits->dna [0 1 2 3]) [:N :U :I :M]))
    (is (= (digits->dna [1 2 0 3] nmui-code) [:N :M :U :I]))
    (is (= (digits->dna '(1 0 3 2  0 1 2 3  3 2 1 0  2 3 0 1) nmui-code)
           [:M :I :U :N  :I :M :N :U  :U :N :M :I  :N :U :I :M]))
    (is (= (digits->dna [2 3 0 1  2 0 0 2  2 3 0 1  2 0 0 2
                         0 3 0 3  2 3 0 1  2 3 0 1  0 3 0 3
                         0 3 0 3  2 0 0 2  2 3 0 1  0 0 0 0
                         2 3 0 1  2 3 0 1  2 3 0 1  2 3 0 1] nmui-code)
           [:N :U :I :M  :N :N :I :I  :N :U :N :U  :N :N :N :N
            :N :U :I :M  :N :U :I :M  :N :U :N :U  :N :U :N :U
            :N :U :I :M  :N :N :I :I  :N :U :I :M  :N :N :I :I
            :N :U :I :M  :N :U :I :M  :N :U :I :M  :N :U :I :M]))))


(deftest reorder-dna-seq-test
  (testing "Correctness of reordered dna-seq"
    (is (= (reorder-dna-seq
            [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N]
            nuim-code nmui-code)
           [:I :I :I :I :U :U :U :U :M :M :M :M :N :N :N :N]))
    (is (= (reorder-dna-seq
            '(:I :I :I :I :U :U :U :U :M :M :M :M :N :N :N :N)
            nmui-code nuim-code)
           [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N]))))


(deftest compare-dna-test
  (testing "Correctness of sorted order"
    (is (= (sort compare-dna [[:N :M :U :I] [:N :U :I :M] [:M]])
           [[:M] [:N :U :I :M] [:N :M :U :I]]))
    (is (= (sort compare-dna [[[:N :U :I :M] [:I :M]] [[:U :M :I] [:M :N :U]]])
           [[[:U :M :I] [:M :N :U]] [[:N :U :I :M] [:I :M]]]))))


(deftest expand-dna-seq-test
  (testing "Correctness of expansion"
    (is (= (expand-dna-seq [:M :I :U :N] 2)
           [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N]))
    (is (= (expand-dna-seq [:N :U :I :M
                            :U :I :M :I
                            :I :M :I :U
                            :M :I :U :N] 3)
           (expand-dna-seq (list :N :U :I :M
                                 :U :I :M :I
                                 :I :M :I :U
                                 :M :I :U :N) 3)
           [:N :N :N :N :U :U :U :U :I :I :I :I :M :M :M :M
            :U :U :U :U :I :I :I :I :M :M :M :M :I :I :I :I
            :I :I :I :I :M :M :M :M :I :I :I :I :U :U :U :U
            :M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N]))))

(deftest reduce-dna-seq-test
  (testing "Correct reduction for 3->2 dimensions"
    (is (= (reduce-dna-seq '[a b c]
                           [0 0 0 0  1 1 1 1  2 2 2 2  3 3 3 3
                            1 1 1 1  2 2 2 2  3 3 3 3  0 0 0 0
                            2 2 2 2  3 3 3 3  0 0 0 0  1 1 1 1
                            3 3 3 3  0 0 0 0  1 1 1 1  2 2 2 2])
           '[[a b] [0 1 2 3  1 2 3 0  2 3 0 1  3 0 1 2]]))
    (is (= (reduce-dna-seq '[a b c]
                           [0 1 2 3  0 1 2 3  0 1 2 3  0 1 2 3
                            1 2 3 0  1 2 3 0  1 2 3 0  1 2 3 0
                            2 3 0 1  2 3 0 1  2 3 0 1  2 3 0 1
                            3 0 1 2  3 0 1 2  3 0 1 2  3 0 1 2])
           '[[a c] [0 1 2 3  1 2 3 0  2 3 0 1  3 0 1 2]]))
    (is (= (reduce-dna-seq '[a b c]
                           [0 1 2 3  1 2 3 0  2 3 0 1  3 0 1 2
                            0 1 2 3  1 2 3 0  2 3 0 1  3 0 1 2
                            0 1 2 3  1 2 3 0  2 3 0 1  3 0 1 2
                            0 1 2 3  1 2 3 0  2 3 0 1  3 0 1 2])
           '[[b c] [0 1 2 3  1 2 3 0  2 3 0 1  3 0 1 2]])))

  (testing "Correct reduction for 2->1 dimensions"
    (is (= (reduce-dna-seq '[a b] [0 0 0 0  1 1 1 1  2 2 2 2  3 3 3 3])
           '[[a] [0 1 2 3]]))
    (is (= (reduce-dna-seq '[a b] [0 1 2 3  0 1 2 3  0 1 2 3  0 1 2 3])
           '[[b] [0 1 2 3]])))

  (testing "Correct reduction for 1->0 dimensions"
    (is (= (reduce-dna-seq '[a] [0 0 0 0])
           '[[] [0]]))
    (is (= (reduce-dna-seq '[a] [1 1 1 1])
           '[[] [1]])))

  (testing "Correct reduction for 3->1 dimensions"
    (is (= (reduce-dna-seq
            '[a b c]
            [:N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N
             :U :U :U :U  :U :U :U :U  :U :U :U :U  :U :U :U :U
             :I :I :I :I  :I :I :I :I  :I :I :I :I  :I :I :I :I
             :M :M :M :M  :M :M :M :M  :M :M :M :M  :M :M :M :M])
           (reduce-dna-seq
            '[c b a]
            [:N :U :I :M  :N :U :I :M  :N :U :I :M  :N :U :I :M
             :N :U :I :M  :N :U :I :M  :N :U :I :M  :N :U :I :M
             :N :U :I :M  :N :U :I :M  :N :U :I :M  :N :U :I :M
             :N :U :I :M  :N :U :I :M  :N :U :I :M  :N :U :I :M])
           (reduce-dna-seq
            '[b a c]
            [:N :N :N :N  :U :U :U :U  :I :I :I :I  :M :M :M :M
             :N :N :N :N  :U :U :U :U  :I :I :I :I  :M :M :M :M
             :N :N :N :N  :U :U :U :U  :I :I :I :I  :M :M :M :M
             :N :N :N :N  :U :U :U :U  :I :I :I :I  :M :M :M :M])
           '[[a] [:N :U :I :M]])))

  (testing "Correct reduction for n->0 dimensions"
    (is (= (reduce-dna-seq
            '[a b c d]
            [:N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N
             :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N
             :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N
             :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N

             :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N
             :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N
             :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N
             :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N

             :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N
             :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N
             :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N
             :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N

             :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N
             :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N
             :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N
             :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N])
           (reduce-dna-seq
            '[a b c]
            [:N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N
             :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N
             :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N
             :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N])
           (reduce-dna-seq
            [:N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N])
           (reduce-dna-seq ['x] [:N :N :N :N])
           (reduce-dna-seq [:N])
           [[] [:N]])))

  (testing "Irreducable formDNA"
    (is (let [dna [:N :U :I :M  :U :I :M :I  :I :M :I :U  :M :I :U :N
                   :U :I :M :U  :I :M :I :I  :M :I :U :M  :I :U :N :N
                   :I :M :U :I  :M :I :I :M  :I :U :M :I  :U :N :N :U
                   :M :U :I :M  :I :I :M :I  :U :M :I :U  :N :N :U :I]]
          (= (reduce-dna-seq dna) [[0 1 2] dna]))))

  (testing "Almost possible reductions"
    (is (let [dna [:N :U :N :N]] (= (reduce-dna-seq dna) [[0] dna])))
    (is (let [dna [:N :N :N :N
                   :N :N :N :N
                   :N :N :N :N
                   :N :N :N :U]] (= (reduce-dna-seq dna) [[0 1] dna])))
    (is (let [dna [:U :U :U :U  :U :U :U :U  :U :U :U :U  :U :U :U :U
                   :U :I :U :U  :U :U :U :U  :U :U :U :U  :U :U :U :U
                   :U :U :U :U  :U :U :U :U  :U :U :U :U  :U :U :U :U
                   :U :U :U :U  :U :U :U :U  :U :U :U :U  :U :U :U :U]]
          (= (reduce-dna-seq dna) [[0 1 2] dna])))
    (is (let [dna [:U :I :U :U  :U :I :U :U  :U :I :U :U  :U :I :U :U
                   :U :I :I :U  :U :I :I :U  :U :I :I :U  :U :I :I :U
                   :U :I :U :U  :U :I :U :U  :U :I :U :U  :U :I :U :U
                   :U :I :U :U  :U :I :U :U  :U :I :U :U  :U :I :U :U]]
          (= (reduce-dna-seq dna)
             [[0 2] [:U :I :U :U
                     :U :I :I :U
                     :U :I :U :U
                     :U :I :U :U]])))
    (is (let [dna [:N :N :N :N  :U :U :U :U  :I :I :I :I  :M :M :M :M
                   :N :N :N :N  :U :U :U :U  :I :I :I :I  :M :M :M :M
                   :N :N :N :N  :U :U :U :U  :I :I :I :I  :M :M :M :M
                   :N :N :N :N  :I :I :I :I  :I :I :I :I  :M :M :M :M]]
          (= (reduce-dna-seq dna)
             [[0 1] [:N :U :I :M
                     :N :U :I :M
                     :N :U :I :M
                     :N :I :I :M]])))
    (is (let [dna [:N :U :I :M  :N :U :I :M  :N :U :I :M  :N :U :I :N
                   :N :U :I :M  :N :U :I :M  :N :U :I :M  :N :U :I :N
                   :N :U :I :M  :N :U :I :M  :N :U :I :M  :N :U :I :N
                   :N :U :I :M  :N :U :I :M  :N :U :I :M  :N :U :I :N]]
          (= (reduce-dna-seq dna)
             [[1 2] [:N :U :I :M  :N :U :I :M  :N :U :I :M  :N :U :I :N]]))))

  (testing "Equivalence of reduction paths"
    (is (= (reduce-dna-seq [:N :U :I :M])
           (reduce-dna-seq [:N :U :I :M])
           [[0] [:N :U :I :M]]))
    (is (= (reduce-dna-seq [:U])
           (reduce-dna-seq [:U :U :U :U])
           (reduce-dna-seq [:U :U :U :U
                            :U :U :U :U
                            :U :U :U :U
                            :U :U :U :U])
           (reduce-dna-seq
            [:U :U :U :U  :U :U :U :U  :U :U :U :U  :U :U :U :U
             :U :U :U :U  :U :U :U :U  :U :U :U :U  :U :U :U :U
             :U :U :U :U  :U :U :U :U  :U :U :U :U  :U :U :U :U
             :U :U :U :U  :U :U :U :U  :U :U :U :U  :U :U :U :U])
           [[] [:U]]))
    (is (= (reduce-dna-seq [1] [:U :I :N :M])
           (reduce-dna-seq [:U :I :N :M
                            :U :I :N :M
                            :U :I :N :M
                            :U :I :N :M])
           (reduce-dna-seq [1 0] [:U :U :U :U
                                  :I :I :I :I
                                  :N :N :N :N
                                  :M :M :M :M])
           [[1] [:U :I :N :M]]))
    (is (= (reduce-dna-seq [:U :I :N :M])
           (reduce-dna-seq [:U :U :U :U
                            :I :I :I :I
                            :N :N :N :N
                            :M :M :M :M])
           (reduce-dna-seq [1 0] [:U :I :N :M
                                  :U :I :N :M
                                  :U :I :N :M
                                  :U :I :N :M])
           [[0] [:U :I :N :M]]))
    )) 


(deftest equiv-dna-test
  (testing "Equivalence of identity"
    (are [x] (equiv-dna x)
      [:N] [:N :U :I :M] [:N :U :I :M
                          :U :I :M :N
                          :I :M :N :U
                          :M :N :U :I])
    (are [x] (equiv-dna x x)
      [:N] [:U] [:I] [:M])
    (are [x] (equiv-dna x x)
      [:N :N :N :N] [:U :U :U :U] [:I :I :I :I] [:M :M :M :M])
    (are [x] (equiv-dna x x)
      [:N :U :I :M] [:N :U :I :M
                     :U :I :M :N
                     :I :M :N :U
                     :M :N :U :I])
    (is (equiv-dna [:N :U :I :M] '(:N :U :I :M))))

  (testing "Non-equivalence"
    (is (not (equiv-dna [:U] [:I]))) (is (not (equiv-dna [:M] [:N])))
    (is (not (equiv-dna [:M] [:U]))) (is (not (equiv-dna [:M] [:I])))
    (is (not (equiv-dna [:N] [:U]))) (is (not (equiv-dna [:N] [:I])))
    (is (not (equiv-dna [:N] [:N :N :N :M])))
    (is (not (equiv-dna [:N :N :N :N] [:N :N :N :M])))
    (is (not (equiv-dna [:N :N :N :N :U :U :U :U :U :U :I :I :U :I :I :M]
                        [:N :U :U :U :N :U :U :I :N :U :I :I :N :U :I :N])))
    )

  (testing "Equivalence of permutation"
    (are [x] (apply equiv-dna (vals (dna-perspectives x)))
      [:M :I :U :N :M :M :U :U :M :I :M :I :M :M :M :M]
      [:M :I :U :N :I :I :N :N :U :N :U :N :N :N :N :N :M :I :U :N :M :I :U :N :U :N :U :N :U :N :U :N :M :I :U :N :I :I :N :N :M :I :U :N :I :I :N :N :M :I :U :N :M :I :U :N :M :I :U :N :M :I :U :N]))

  (testing "Equivalence of tautology reduction"
    (is (equiv-dna [:N] (repeat 4 :N) (repeat 16 :N) (repeat 64 :N)))
    (is (equiv-dna [:U] (repeat 4 :U) (repeat 16 :U) (repeat 64 :U)))
    (is (equiv-dna [:I] (repeat 4 :I) (repeat 16 :I) (repeat 64 :I)))
    (is (equiv-dna [:M] (repeat 4 :M) (repeat 16 :M) (repeat 64 :M)))
    (is (equiv-dna [:N :U :I :M]
                   [:N :U :I :M :N :U :I :M :N :U :I :M :N :U :I :M]))
    (is (equiv-dna [:N :N :N :N  :U :U :U :U  :I :I :I :I  :M :M :M :M
                    :N :N :N :N  :N :N :N :N  :I :I :I :I  :I :I :I :I
                    :N :N :N :N  :U :U :U :U  :N :N :N :N  :U :U :U :U
                    :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N]
                   [:N :U :I :M
                    :N :N :I :I
                    :N :U :N :U
                    :N :N :N :N]
                   ; permutation:
                   [:N :U :I :M  :N :U :I :M  :N :U :I :M  :N :U :I :M
                    :N :N :I :I  :N :N :I :I  :N :N :I :I  :N :N :I :I
                    :N :U :N :U  :N :U :N :U  :N :U :N :U  :N :U :N :U
                    :N :N :N :N  :N :N :N :N  :N :N :N :N  :N :N :N :N]
                   [:N :U :I :M
                    :N :N :I :I
                    :N :U :N :U
                    :N :N :N :N]))
    (is (equiv-dna [:N :N :N :N :N :N :N :N :N :N :N :N :N :N :N :N :U :U :U :U :U :U :U :U :U :U :U :U :U :U :U :U :U :U :I :I :U :U :I :I :U :U :I :I :U :U :I :I :U :I :I :M :U :I :I :M :U :I :I :M :U :I :I :M :N :N :N :N :N :N :N :N :N :N :N :N :N :N :N :N :U :U :U :U :U :U :U :U :U :U :U :U :U :U :U :U :U :U :I :I :U :U :I :I :U :U :I :I :U :U :I :I :U :I :I :M :U :I :I :M :U :I :I :M :U :I :I :M :N :N :N :N :N :N :N :N :N :N :N :N :N :N :N :N :U :U :U :U :U :U :U :U :U :U :U :U :U :U :U :U :U :U :I :I :U :U :I :I :U :U :I :I :U :U :I :I :U :I :I :M :U :I :I :M :U :I :I :M :U :I :I :M :N :N :N :N :N :N :N :N :N :N :N :N :N :N :N :N :U :U :U :U :U :U :U :U :U :U :U :U :U :U :U :U :U :U :I :I :U :U :I :I :U :U :I :I :U :U :I :I :U :I :I :M :U :I :I :M :U :I :I :M :U :I :I :M]
                   [:N :N :N :N :U :U :U :U :U :U :I :I :U :I :I :M]
                   [:N :U :U :U :N :U :U :U :N :U :U :U :N :U :U :U :N :U :U :U :N :U :U :U :N :U :U :U :N :U :U :U :N :U :U :U :N :U :U :U :N :U :U :U :N :U :U :U :N :U :U :U :N :U :U :U :N :U :U :U :N :U :U :U :N :U :U :I :N :U :U :I :N :U :U :I :N :U :U :I :N :U :U :I :N :U :U :I :N :U :U :I :N :U :U :I :N :U :U :I :N :U :U :I :N :U :U :I :N :U :U :I :N :U :U :I :N :U :U :I :N :U :U :I :N :U :U :I :N :U :I :I :N :U :I :I :N :U :I :I :N :U :I :I :N :U :I :I :N :U :I :I :N :U :I :I :N :U :I :I :N :U :I :I :N :U :I :I :N :U :I :I :N :U :I :I :N :U :I :I :N :U :I :I :N :U :I :I :N :U :I :I :N :U :I :M :N :U :I :M :N :U :I :M :N :U :I :M :N :U :I :M :N :U :I :M :N :U :I :M :N :U :I :M :N :U :I :M :N :U :I :M :N :U :I :M :N :U :I :M :N :U :I :M :N :U :I :M :N :U :I :M :N :U :I :M]
                   [:N :U :U :U :N :U :U :I :N :U :I :I :N :U :I :M])))
  )


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

(deftest filter-dna-test
  (testing "Correctness of transformation"
    (is (= (filter-dna [:N :M :U :I] [:U]) [:U]))
    (is (= (filter-dna [:N :M :U :I] [:M]) [:N]))
    (is (= (filter-dna [:N :U :I :M  :U :I :M :I  :I :M :I :U  :M :I :U :N
                        :U :I :M :U  :I :M :I :I  :M :I :U :M  :I :U :N :N
                        :I :M :U :I  :M :I :I :M  :I :U :M :I  :U :N :N :U
                        :M :U :I :M  :I :I :M :I  :U :M :I :U  :N :N :U :I]
                       [:_ :I :_]) [:U :I :M :I
                                    :I :M :I :I
                                    :M :I :I :M
                                    :I :I :M :I]))))


(deftest dna->quaternary-test
  (testing "Correctness of conversion"
    (is (= "4r0123" (dna->quaternary [:N :U :I :M])))
    (is (= "4r1232232232232232" (dna->quaternary [:U :I :M :I
                                                  :I :M :I :I
                                                  :M :I :I :M
                                                  :I :I :M :I])))
    (is (= "4r0312" (dna->quaternary [:N :M :U :I])))
    ;; should this return nil?
    (is (thrown? clojure.lang.ExceptionInfo
                 (dna->quaternary [:I :M])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (dna->quaternary [])))))


(deftest permute-dna-seq-test
  (testing "Correctness of permutation"
    (is (= (permute-dna-seq
            [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N] [1 0])
           [[1 0] [:M :I :U :N :M :I :U :N :M :I :U :N :M :I :U :N]]))))


(deftest dna-seq-perspectives-test
  (testing "Correctness of permutations"
    (is (= (dna-seq-perspectives
            [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N])
           [[[0 1] [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N]]
            [[1 0] [:M :I :U :N :M :I :U :N :M :I :U :N :M :I :U :N]]]))))

(deftest dna-perspectives-test
  (testing "Correctness of permutations"
    (is (= (dna-perspectives (make-dna (make-dna :U :I :M :N)
                                       (make-dna :U :I :M :N)
                                       (make-dna :U :I :M :N)
                                       (make-dna :U :I :M :N)))
           {[0 1] [:U :I :M :N :U :I :M :N :U :I :M :N :U :I :M :N],
            [1 0] [:U :U :U :U :I :I :I :I :M :M :M :M :N :N :N :N]}))))


(deftest vpoint-test
  (testing "Validity of vpoint"
    (is (vpoint? []))
    (is (vpoint? [:N]))
    (is (vpoint? [:N :U]))
    (is (not (vpoint? #{:N :U}))))) 

(deftest rand-vpoint-test
  (testing "Validity of vpoint"
    (is (vpoint? (rand-vpoint 0)))
    (is (vpoint? (rand-vpoint 1)))
    (is (vpoint? (rand-vpoint 5)))
    (take 10 (rand-vpoint))))


(deftest vspace-test
  (testing "Validity of vspace"
    (is (vspace? [[:N] [:I] [:U] [:M]]))
    (is (vspace? #{[:N] [:I] [:U] [:M]})) ;; ? can be a set
    (is (not (vspace? [[:N :N] [:N :U] [:N :I] [:I :U]])))
    (is (vspace? (vspace 3 nuim-code)))
    (is (vspace? (vspace 3 nmui-code)))))


(deftest vdict-test
  (testing "Validity of vdict"
    (is (vdict? (let [vp->r {[:N :M] :M
                             [:U :U] :I
                             ; [:X :Y] :M
                             [:U :U :I] :N}]
                  (vdict vp->r {:default-result :U}))))) )


(deftest dna->vdict-test
  (testing "Validity of vdict"
    (is (vdict? (dna->vdict (rand-dna 4) {:sorted? true})))))


(deftest vdict->vmap-test
  (testing "Validity of vmap"
    (is (vmap? (vdict->vmap (dna->vdict (rand-dna 3) {}))))))

(deftest rel-test
  (testing "Correctness of relation"
    (is (= :N (rel) (rel :N) (rel :N :N)))
    (is (= :U (rel :U) (rel :U :U) (rel :N :U)))
    (is (= :I (rel :I) (rel :I :I) (rel :N :I)))
    (is (= :M (rel :M) (rel :M :M) (rel :N :M)
           (rel :M :N) (rel :M :U) (rel :M :I) (rel :U :I))))

  (testing "Multiple arguments"
    (is (= :M (rel :U :I :U :N))))

  (testing "formDNA relation"
    (is (= (rel [:N :M :U :I] [:M :N :I :N]) [:M :M :M :I]))
    (is (= (rel [:N :M :U :I  :I :U :N :M  :M :I :I :I  :U :U :M :N]
                [:N :U :I :M] [:N :U :I :M])
           [:N :M :U :I  :M :U :U :M  :M :I :I :I  :M :M :M :M]))
    (is (= (rel
            [:N :U :I :M  :N :N :I :I  :N :U :N :U  :N :N :N :N
             :N :U :I :M  :N :U :I :M  :N :U :N :U  :N :U :N :U
             :N :U :I :M  :N :N :I :I  :N :U :I :M  :N :N :I :I
             :N :U :I :M  :N :U :I :M  :N :U :I :M  :N :U :I :M]
            [:M :I :U :N  :I :M :N :U  :U :N :M :I  :N :U :I :M])
           [:M :M :M :M  :I :I :I :I  :U :U :U :U  :N :N :N :N
            :I :M :I :M  :M :M :M :M  :N :U :N :U  :U :U :U :U
            :U :U :M :M  :N :N :I :I  :M :M :M :M  :I :I :I :I
            :N :U :I :M  :U :U :M :M  :I :M :I :M  :M :M :M :M]))))


(deftest inv-test
  (testing "Correctness of inversion"
    (is (= :M (inv) (inv :N)))
    (is (= :I (inv :U)))
    (is (= :U (inv :I)))
    (is (= :N (inv :M))))

  (testing "formDNA inversion"
    (is (= [:M :I :U :N] (inv [:N :U :I :M])))
    (is (= [:M :N :I :U  :U :I :M :N  :N :U :U :U  :I :I :N :M]
           (inv [:N :M :U :I  :I :U :N :M  :M :I :I :I  :U :U :M :N])))))
