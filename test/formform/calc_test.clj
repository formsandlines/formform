;; VVV
(ns formform.calc-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [formform.calc :refer :all]
            [orchestra.spec.test :as stest]))

(doseq [fsym fns-with-specs] (stest/instrument fsym))

(def rv (comp vec reverse))

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
                 (rand-dna 1 [])))))
    


;; VVV
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


;; VVV
(deftest dna->digits-test
  (testing "Correctness of conversion"
    (is (= (dna->digits [:N :U :I :M]) '(0 1 2 3)))
    (is (= (dna->digits nmui-code [:N :U :I :M]) '(0 1 2 3)))
    (is (= (dna->digits
            nmui-code
            [:M :M :M :M  :M :I :M :I  :M :M :U :U  :M :I :U :N
             :I :I :I :I  :M :M :M :M  :I :I :N :N  :M :M :U :U
             :U :U :U :U  :U :N :U :N  :M :M :M :M  :M :I :M :I
             :N :N :N :N  :U :U :U :U  :I :I :I :I  :M :M :M :M])
           [1 1 1 1  1 0 3 2  1 3 3 1  1 2 1 2
            0 0 0 0  1 1 1 1  2 2 2 2  3 3 3 3
            3 3 3 3  1 2 1 2  1 1 1 1  3 0 3 0  
            2 2 2 2  1 3 3 1  2 0 0 2  1 1 1 1]))))


;; VVV
(deftest digits->dna-test
  (testing "Correctness of conversion"
    (is (= (digits->dna [0 1 2 3]) [:N :U :I :M]))
    (is (= (digits->dna nmui-code [1 2 0 3]) [:M :N :I :U]))
    (is (= (digits->dna nmui-code '(1 0 3 2  0 1 2 3  3 2 1 0  2 3 0 1))
           [:M :I :U :N  :I :M :N :U  :U :N :M :I  :N :U :I :M]))
    (is (= (digits->dna nmui-code
                        [2 3 0 1  2 0 0 2  2 3 0 1  2 0 0 2
                         0 3 0 3  2 3 0 1  2 3 0 1  0 3 0 3
                         0 3 0 3  2 0 0 2  2 3 0 1  0 0 0 0
                         2 3 0 1  2 3 0 1  2 3 0 1  2 3 0 1])
           [:U :N :M :I  :U :N :M :I  :U :N :U :N  :U :N :U :N
            :N :N :I :I  :U :N :M :I  :N :N :N :N  :U :N :U :N
            :U :N :M :I  :U :N :M :I  :U :N :M :I  :U :N :M :I
            :N :N :I :I  :U :N :M :I  :N :N :I :I  :U :N :M :I]))))


;; VVV
(deftest reorder-dna-seq-test
  (testing "Correctness of reordered dna-seq"
    (is (= (reorder-dna-seq
            [:N :N :N :N :U :U :U :U :I :I :I :I :M :M :M :M]
            nuim-code nmui-code)
           [:N :N :N :N :M :M :M :M :U :U :U :U :I :I :I :I]))
    (is (= (reorder-dna-seq
            [:N :U :I :M  :U :I :M :N  :I :M :N :U  :M :N :U :I]
            nuim-code nmui-code)
           [:N :M :U :I  :M :I :N :U  :U :N :I :M  :I :U :M :N]))
    (is (= (reorder-dna-seq
            [:N :N :N :N  :U :U :U :U  :I :I :I :I  :M :M :M :M
             :U :U :U :U  :I :I :I :I  :M :M :M :M  :N :N :N :N
             :I :I :I :I  :M :M :M :M  :N :N :N :N  :U :U :U :U
             :M :M :M :M  :N :N :N :N  :U :U :U :U  :I :I :I :I]
            nuim-code nmui-code)
           [:N :N :N :N  :M :M :M :M  :U :U :U :U  :I :I :I :I
            :M :M :M :M  :I :I :I :I  :N :N :N :N  :U :U :U :U
            :U :U :U :U  :N :N :N :N  :I :I :I :I  :M :M :M :M
            :I :I :I :I  :U :U :U :U  :M :M :M :M  :N :N :N :N]))
    (is (= (reorder-dna-seq
            [:N :U :I :M  :U :I :M :N  :I :M :N :U  :M :N :U :I
             :U :I :M :N  :I :M :N :U  :M :N :U :I  :N :U :I :M
             :I :M :N :U  :M :N :U :I  :N :U :I :M  :U :I :M :N
             :M :N :U :I  :N :U :I :M  :U :I :M :N  :I :M :N :U]
            nuim-code nmui-code)
           [:N :M :U :I  :M :I :N :U  :U :N :I :M  :I :U :M :N
            :M :I :N :U  :I :U :M :N  :N :M :U :I  :U :N :I :M  
            :U :N :I :M  :N :M :U :I  :I :U :M :N  :M :I :N :U
            :I :U :M :N  :U :N :I :M  :M :I :N :U  :N :M :U :I]))
    (is (= (reorder-dna-seq
            [:N :U :I :M  :U :I :M :N  :I :M :N :U  :M :N :U :I]
            nuim-code nuim-code)
           [:N :U :I :M  :U :I :M :N  :I :M :N :U  :M :N :U :I])))
  (testing "List reorders exactly the same as vector"
    (is (= (reorder-dna-seq
            '(:N :U :I :M  :U :I :M :N  :I :M :N :U  :M :N :U :I)
            nmui-code nuim-code)
           [:N :I :M :U  :I :N :U :M  :M :U :I :N  :U :M :N :I]))))


;; VVV
(deftest compare-consts-test
  (testing "Correctness of sorted order"
    (is (= [] (sort compare-consts [])))
    (is (= [:N] (sort compare-consts [:N])))
    (is (= [:N :M] (sort compare-consts [:M :N])))
    (is (= [:N :U :I :M :M] (sort compare-consts [:U :M :N :I :M]))))
  (testing "Correct order of compared sequences"
    (is (= [[:M] [:N :U :I :M] [:N :M :U :I]]
           (sort compare-consts
                 [[:N :M :U :I] [:N :U :I :M] [:M]])))
    (is (= [[:I :M] [:N :U :I :M] [:U :M :I] [:M :N :U]]
           (sort compare-consts
                 [[:N :U :I :M] [:I :M] [:U :M :I] [:M :N :U]])))))
  

;; VVV
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

;; VVV
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
           [[0] [:U :I :N :M]]))))
    


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
                        [:N :U :U :U :N :U :U :I :N :U :I :I :N :U :I :N]))))
    

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
                   [:N :U :U :U :N :U :U :I :N :U :I :I :N :U :I :M]))))
  


;; VVV
(deftest filter-dna-test
  (testing "Correctness of transformation"
    (is (= (filter-dna [:N] []) [:N]))
    (is (= (filter-dna [:I] []) [:I]))
    (is (= (filter-dna [:I :U :M :N] [:_]) [:I :U :M :N]))
    (is (= (filter-dna [:I :U :M :N] [:U]) [:U]))
    (is (= (filter-dna [:I :U :M :N] [:M]) [:N]))
    (is (= (filter-dna [:I :U :N :N  :U :I :M :U  :I :M :I :I  :M :I :U :M
                        :U :N :N :U  :I :M :U :I  :M :I :I :M  :I :U :M :I
                        :N :N :U :I  :M :U :I :M  :I :I :M :I  :U :M :I :U
                        :N :U :I :M  :U :I :M :I  :I :M :I :U  :M :I :U :N]
                       [:_ :I :_]) [:I :M :I :I
                                    :M :I :I :M
                                    :I :I :M :I
                                    :I :M :I :U]))))

;; VVV
(deftest dna-get-test
  ;; is just filter-dna without the vector and no holes in vpoint allowed
  (testing "IO shape"
    (is (= (dna-get [:U] [])
           :U))
    (is (= (dna-get [:I :U :U :N  :U :N :U :N  :I :M :N :M  :I :M :M :M]
                    [:M :N])
           :I))
    (is (thrown? clojure.lang.ExceptionInfo
                 (dna-get [:I :U :U :N  :U :N :U :N  :I :M :N :M  :I :M :M :M]
                          [:M :_])))))


;; VVV
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
    (is (vspace? (vspace nuim-code 3)))
    (is (vspace? (vspace nmui-code 3)))))


(deftest vdict-test
  (testing "Validity of vdict"
    (is (vdict? (let [vp->r {[:N :M] :M
                             [:U :U] :I
                             ; [:X :Y] :M
                             [:U :U :I] :N}]
                  (vdict {:default-result :U} vp->r))))))


;; VVV
(deftest dna->vdict-test
  (testing "Validity of vdict"
    (is ((every-pred vdict? sorted?)
         (dna->vdict {:sorted? true} (rand-dna 4)))))
  (testing "Correctness of transformation"
    (is (= (dna->vdict [:M :I :U :N])
           {[:N] :M, [:U] :I, [:I] :U, [:M] :N}))
    (is (= (dna->vdict [:N :M :U :U
                        :I :I :I :M
                        :M :N :U :I
                        :I :U :M :N])
           {[:N :N] :N, [:N :U] :M, [:N :I] :U, [:N :M] :U,
            [:U :N] :I, [:U :U] :I, [:U :I] :I, [:U :M] :M,
            [:I :N] :M, [:I :U] :N, [:I :I] :U, [:I :M] :I,
            [:M :N] :I, [:M :U] :U, [:M :I] :M, [:M :M] :N}))))


(deftest vdict->vmap-test
  (testing "Validity of vmap"
    (is (vmap? (vdict->vmap (dna->vdict {} [:N]))))
    (is (vmap? (vdict->vmap (dna->vdict {} [:U]))))
    (is (vmap? (vdict->vmap (dna->vdict {} [:I]))))
    (is (vmap? (vdict->vmap (dna->vdict {} [:M]))))
    (is (vmap? (vdict->vmap (dna->vdict {} (rand-dna 3))))))
  (testing "Correctness of transformation"
    (is (= :N
           (vdict->vmap {[] :N})))
    (is (= '{:N :M, :U :I, :I :U, :M :N}
           (vdict->vmap {[:N] :M, [:U] :I, [:I] :U, [:M] :N})))
    (is (= '{:N {:N :N, :U :M, :I :U, :M :U},
             :U {:N :I, :U :I, :I :I, :M :M},
             :I {:N :M, :U :N, :I :U, :M :I},
             :M {:N :I, :U :U, :I :M, :M :N}}
           (vdict->vmap {[:N :N] :N, [:N :U] :M, [:N :I] :U, [:N :M] :U,
                         [:U :N] :I, [:U :U] :I, [:U :I] :I, [:U :M] :M,
                         [:I :N] :M, [:I :U] :N, [:I :I] :U, [:I :M] :I,
                         [:M :N] :I, [:M :U] :U, [:M :I] :M, [:M :M] :N})))))

(deftest vmap-dimension-test
  (testing "Cached dimension in vmap meta"
    (is (= 5 (vmap-dimension (dna->vmap (rand-dna 5))))))
         
  (testing "Matching dimension for vmaps without meta"
    (is (= 0 (vmap-dimension :N)))
    (is (= 0 (vmap-dimension :U)))
    (is (= 1 (vmap-dimension {:N :M, :U :I, :I :U, :M :N})))
    (is (= 2 (vmap-dimension
              {:N {:N :N, :U :M, :I :U, :M :U},
               :U {:N :I, :U :I, :I :I, :M :M},
               :I {:N :M, :U :N, :I :U, :M :I},
               :M {:N :I, :U :U, :I :M, :M :N}}))))) 

;; VVV
(deftest vmap-perspectives-test
  (testing "Correct output shape"
    (is (= {[0 1] {:M {:M :U, :I :I, :U :M, :N :N}
                   :I {:M :U, :I :I, :U :M, :N :N}
                   :U {:M :U, :I :I, :U :M, :N :N}
                   :N {:M :U, :I :I, :U :M, :N :N}}
            [1 0] {:M {:M :U, :I :U, :U :U, :N :U}
                   :I {:M :I, :I :I, :U :I, :N :I}
                   :U {:M :M, :I :M, :U :M, :N :M}
                   :N {:M :N, :I :N, :U :N, :N :N}}}
           (vmap-perspectives
            {[0 1] [:N :M :I :U :N :M :I :U :N :M :I :U :N :M :I :U],
             [1 0] [:N :N :N :N :M :M :M :M :I :I :I :I :U :U :U :U]}))))
  (testing "Retains dna-perspectives metadata"
    (is (= '([0 1 2 3] [0 1 3 2] [0 2 1 3] [0 2 3 1] [0 3 1 2] [0 3 2 1] [1 0 2 3] [1 0 3 2] [1 2 0 3] [1 2 3 0] [1 3 0 2] [1 3 2 0] [2 0 1 3] [2 0 3 1] [2 1 0 3] [2 1 3 0] [2 3 0 1] [2 3 1 0] [3 0 1 2] [3 0 2 1] [3 1 0 2] [3 1 2 0] [3 2 0 1] [3 2 1 0])
           (:sorted-keys
            (meta (vmap-perspectives (dna-perspectives (rand-dna 4)))))))))

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
