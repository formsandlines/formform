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
    (is (dna? [:n :u :u :m]))
    (is (not (dna? :nuum)))
    (is (not (dna? [:_])))
    (is (not (dna? [:n :a :i :m])))
    (is (not (dna? [0 1 2 3])))
    (is (not (dna? ["n"]))))

  (testing "Dimension validity"
    (is (dna? '(:n)))
    (is (dna? [:u :n :i :u
               :i :u :n :u
               :u :u :n :u
               :n :n :u :u]))
    (is (not (dna? '(:m :n))))
    (is (not (dna? '())))
    (is (not (dna? '(:n :u :i))))))


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



(deftest make-dna-test
  (testing "Correct transformation of mixed element types"
    (is (= (make-dna :m \U \I 0 [1 2 1 3] :u :u :i \n [1 2 1 3])
           [:m :u :i :n :u :i :u :m :u :u :i :n :u :i :u :m])
        (= (make-dna \n \1 :u \M
                     :i :u \n \M
                     [:m :u 2 :n]
                     [\n \n \u 3])
           [:n :u :u :m :i :u :n :m :m :u :i :n :n :n :u :m])))
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
    (is (= (dna->digits [:n :u :i :m]) '(0 1 2 3)))
    (is (= (dna->digits nmui-code [:n :u :i :m]) '(0 1 2 3)))
    (is (= (dna->digits
            nmui-code
            [:m :m :m :m  :m :i :m :i  :m :m :u :u  :m :i :u :n
             :i :i :i :i  :m :m :m :m  :i :i :n :n  :m :m :u :u
             :u :u :u :u  :u :n :u :n  :m :m :m :m  :m :i :m :i
             :n :n :n :n  :u :u :u :u  :i :i :i :i  :m :m :m :m])
           [1 1 1 1  1 0 3 2  1 3 3 1  1 2 1 2
            0 0 0 0  1 1 1 1  2 2 2 2  3 3 3 3
            3 3 3 3  1 2 1 2  1 1 1 1  3 0 3 0  
            2 2 2 2  1 3 3 1  2 0 0 2  1 1 1 1]))))


(deftest digits->dna-test
  (testing "Correctness of conversion"
    (is (= (digits->dna [0 1 2 3]) [:n :u :i :m]))
    (is (= (digits->dna nmui-code [1 2 0 3]) [:m :n :i :u]))
    (is (= (digits->dna nmui-code '(1 0 3 2  0 1 2 3  3 2 1 0  2 3 0 1))
           [:m :i :u :n  :i :m :n :u  :u :n :m :i  :n :u :i :m]))
    (is (= (digits->dna nmui-code
                        [2 3 0 1  2 0 0 2  2 3 0 1  2 0 0 2
                         0 3 0 3  2 3 0 1  2 3 0 1  0 3 0 3
                         0 3 0 3  2 0 0 2  2 3 0 1  0 0 0 0
                         2 3 0 1  2 3 0 1  2 3 0 1  2 3 0 1])
           [:u :n :m :i  :u :n :m :i  :u :n :u :n  :u :n :u :n
            :n :n :i :i  :u :n :m :i  :n :n :n :n  :u :n :u :n
            :u :n :m :i  :u :n :m :i  :u :n :m :i  :u :n :m :i
            :n :n :i :i  :u :n :m :i  :n :n :i :i  :u :n :m :i]))))


(deftest reorder-dna-seq-test
  (testing "Correctness of reordered dna-seq"
    (is (= (reorder-dna-seq
            [:n :n :n :n :u :u :u :u :i :i :i :i :m :m :m :m]
            nuim-code nmui-code)
           [:n :n :n :n :m :m :m :m :u :u :u :u :i :i :i :i]))
    (is (= (reorder-dna-seq
            [:n :u :i :m  :u :i :m :n  :i :m :n :u  :m :n :u :i]
            nuim-code nmui-code)
           [:n :m :u :i  :m :i :n :u  :u :n :i :m  :i :u :m :n]))
    (is (= (reorder-dna-seq
            [:n :n :n :n  :u :u :u :u  :i :i :i :i  :m :m :m :m
             :u :u :u :u  :i :i :i :i  :m :m :m :m  :n :n :n :n
             :i :i :i :i  :m :m :m :m  :n :n :n :n  :u :u :u :u
             :m :m :m :m  :n :n :n :n  :u :u :u :u  :i :i :i :i]
            nuim-code nmui-code)
           [:n :n :n :n  :m :m :m :m  :u :u :u :u  :i :i :i :i
            :m :m :m :m  :i :i :i :i  :n :n :n :n  :u :u :u :u
            :u :u :u :u  :n :n :n :n  :i :i :i :i  :m :m :m :m
            :i :i :i :i  :u :u :u :u  :m :m :m :m  :n :n :n :n]))
    (is (= (reorder-dna-seq
            [:n :u :i :m  :u :i :m :n  :i :m :n :u  :m :n :u :i
             :u :i :m :n  :i :m :n :u  :m :n :u :i  :n :u :i :m
             :i :m :n :u  :m :n :u :i  :n :u :i :m  :u :i :m :n
             :m :n :u :i  :n :u :i :m  :u :i :m :n  :i :m :n :u]
            nuim-code nmui-code)
           [:n :m :u :i  :m :i :n :u  :u :n :i :m  :i :u :m :n
            :m :i :n :u  :i :u :m :n  :n :m :u :i  :u :n :i :m  
            :u :n :i :m  :n :m :u :i  :i :u :m :n  :m :i :n :u
            :i :u :m :n  :u :n :i :m  :m :i :n :u  :n :m :u :i]))
    (is (= (reorder-dna-seq
            [:n :u :i :m  :u :i :m :n  :i :m :n :u  :m :n :u :i]
            nuim-code nuim-code)
           [:n :u :i :m  :u :i :m :n  :i :m :n :u  :m :n :u :i])))
  (testing "List reorders exactly the same as vector"
    (is (= (reorder-dna-seq
            '(:n :u :i :m  :u :i :m :n  :i :m :n :u  :m :n :u :i)
            nmui-code nuim-code)
           [:n :i :m :u  :i :n :u :m  :m :u :i :n  :u :m :n :i]))))


(deftest compare-consts-test
  (testing "Correctness of sorted order"
    (is (= [] (sort compare-consts [])))
    (is (= [:n] (sort compare-consts [:n])))
    (is (= [:n :m] (sort compare-consts [:m :n])))
    (is (= [:n :u :i :m :m] (sort compare-consts [:u :m :n :i :m]))))
  (testing "correct order of compared sequences"
    (is (= [[:m] [:n :u :i :m] [:n :m :u :i]]
           (sort compare-consts
                 [[:n :m :u :i] [:n :u :i :m] [:m]])))
    (is (= [[:i :m] [:n :u :i :m] [:u :m :i] [:m :n :u]]
           (sort compare-consts
                 [[:n :u :i :m] [:i :m] [:u :m :i] [:m :n :u]])))))


(deftest expand-dna-seq-test
  (testing "Correctness of expansion"
    (is (= (expand-dna-seq [:m :i :u :n] 2)
           [:m :m :m :m :i :i :i :i :u :u :u :u :n :n :n :n]))
    (is (= (expand-dna-seq [:n :u :i :m
                            :u :i :m :i
                            :i :m :i :u
                            :m :i :u :n] 3)
           (expand-dna-seq (list :n :u :i :m
                                 :u :i :m :i
                                 :i :m :i :u
                                 :m :i :u :n) 3)
           [:n :n :n :n :u :u :u :u :i :i :i :i :m :m :m :m
            :u :u :u :u :i :i :i :i :m :m :m :m :i :i :i :i
            :i :i :i :i :m :m :m :m :i :i :i :i :u :u :u :u
            :m :m :m :m :i :i :i :i :u :u :u :u :n :n :n :n]))))

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
            [:n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n
             :u :u :u :u  :u :u :u :u  :u :u :u :u  :u :u :u :u
             :i :i :i :i  :i :i :i :i  :i :i :i :i  :i :i :i :i
             :m :m :m :m  :m :m :m :m  :m :m :m :m  :m :m :m :m])
           (reduce-dna-seq
            '[c b a]
            [:n :u :i :m  :n :u :i :m  :n :u :i :m  :n :u :i :m
             :n :u :i :m  :n :u :i :m  :n :u :i :m  :n :u :i :m
             :n :u :i :m  :n :u :i :m  :n :u :i :m  :n :u :i :m
             :n :u :i :m  :n :u :i :m  :n :u :i :m  :n :u :i :m])
           (reduce-dna-seq
            '[b a c]
            [:n :n :n :n  :u :u :u :u  :i :i :i :i  :m :m :m :m
             :n :n :n :n  :u :u :u :u  :i :i :i :i  :m :m :m :m
             :n :n :n :n  :u :u :u :u  :i :i :i :i  :m :m :m :m
             :n :n :n :n  :u :u :u :u  :i :i :i :i  :m :m :m :m])
           '[[a] [:n :u :i :m]])))

  (testing "Correct reduction for n->0 dimensions"
    (is (= (reduce-dna-seq
            '[a b c d]
            [:n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n
             :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n
             :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n
             :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n

             :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n
             :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n
             :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n
             :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n

             :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n
             :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n
             :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n
             :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n

             :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n
             :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n
             :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n
             :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n])
           (reduce-dna-seq
            '[a b c]
            [:n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n
             :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n
             :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n
             :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n])
           (reduce-dna-seq
            [:n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n])
           (reduce-dna-seq ['x] [:n :n :n :n])
           (reduce-dna-seq [:n])
           [[] [:n]])))

  (testing "Irreducable formDNA"
    (is (let [dna [:n :u :i :m  :u :i :m :i  :i :m :i :u  :m :i :u :n
                   :u :i :m :u  :i :m :i :i  :m :i :u :m  :i :u :n :n
                   :i :m :u :i  :m :i :i :m  :i :u :m :i  :u :n :n :u
                   :m :u :i :m  :i :i :m :i  :u :m :i :u  :n :n :u :i]]
          (= (reduce-dna-seq dna) [[0 1 2] dna]))))

  (testing "Almost possible reductions"
    (is (let [dna [:n :u :n :n]] (= (reduce-dna-seq dna) [[0] dna])))
    (is (let [dna [:n :n :n :n
                   :n :n :n :n
                   :n :n :n :n
                   :n :n :n :u]] (= (reduce-dna-seq dna) [[0 1] dna])))
    (is (let [dna [:u :u :u :u  :u :u :u :u  :u :u :u :u  :u :u :u :u
                   :u :i :u :u  :u :u :u :u  :u :u :u :u  :u :u :u :u
                   :u :u :u :u  :u :u :u :u  :u :u :u :u  :u :u :u :u
                   :u :u :u :u  :u :u :u :u  :u :u :u :u  :u :u :u :u]]
          (= (reduce-dna-seq dna) [[0 1 2] dna])))
    (is (let [dna [:u :i :u :u  :u :i :u :u  :u :i :u :u  :u :i :u :u
                   :u :i :i :u  :u :i :i :u  :u :i :i :u  :u :i :i :u
                   :u :i :u :u  :u :i :u :u  :u :i :u :u  :u :i :u :u
                   :u :i :u :u  :u :i :u :u  :u :i :u :u  :u :i :u :u]]
          (= (reduce-dna-seq dna)
             [[0 2] [:u :i :u :u
                     :u :i :i :u
                     :u :i :u :u
                     :u :i :u :u]])))
    (is (let [dna [:n :n :n :n  :u :u :u :u  :i :i :i :i  :m :m :m :m
                   :n :n :n :n  :u :u :u :u  :i :i :i :i  :m :m :m :m
                   :n :n :n :n  :u :u :u :u  :i :i :i :i  :m :m :m :m
                   :n :n :n :n  :i :i :i :i  :i :i :i :i  :m :m :m :m]]
          (= (reduce-dna-seq dna)
             [[0 1] [:n :u :i :m
                     :n :u :i :m
                     :n :u :i :m
                     :n :i :i :m]])))
    (is (let [dna [:n :u :i :m  :n :u :i :m  :n :u :i :m  :n :u :i :n
                   :n :u :i :m  :n :u :i :m  :n :u :i :m  :n :u :i :n
                   :n :u :i :m  :n :u :i :m  :n :u :i :m  :n :u :i :n
                   :n :u :i :m  :n :u :i :m  :n :u :i :m  :n :u :i :n]]
          (= (reduce-dna-seq dna)
             [[1 2] [:n :u :i :m  :n :u :i :m  :n :u :i :m  :n :u :i :n]]))))

  (testing "Equivalence of reduction paths"
    (is (= (reduce-dna-seq [:n :u :i :m])
           (reduce-dna-seq [:n :u :i :m])
           [[0] [:n :u :i :m]]))
    (is (= (reduce-dna-seq [:u])
           (reduce-dna-seq [:u :u :u :u])
           (reduce-dna-seq [:u :u :u :u
                            :u :u :u :u
                            :u :u :u :u
                            :u :u :u :u])
           (reduce-dna-seq
            [:u :u :u :u  :u :u :u :u  :u :u :u :u  :u :u :u :u
             :u :u :u :u  :u :u :u :u  :u :u :u :u  :u :u :u :u
             :u :u :u :u  :u :u :u :u  :u :u :u :u  :u :u :u :u
             :u :u :u :u  :u :u :u :u  :u :u :u :u  :u :u :u :u])
           [[] [:u]]))
    (is (= (reduce-dna-seq [1] [:u :i :n :m])
           (reduce-dna-seq [:u :i :n :m
                            :u :i :n :m
                            :u :i :n :m
                            :u :i :n :m])
           (reduce-dna-seq [1 0] [:u :u :u :u
                                  :i :i :i :i
                                  :n :n :n :n
                                  :m :m :m :m])
           [[1] [:u :i :n :m]]))
    (is (= (reduce-dna-seq [:u :i :n :m])
           (reduce-dna-seq [:u :u :u :u
                            :i :i :i :i
                            :n :n :n :n
                            :m :m :m :m])
           (reduce-dna-seq [1 0] [:u :i :n :m
                                  :u :i :n :m
                                  :u :i :n :m
                                  :u :i :n :m])
           [[0] [:u :i :n :m]]))))



(deftest equiv-dna-test
  (testing "Equivalence of identity"
    (are [x] (equiv-dna x)
      [:n] [:n :u :i :m] [:n :u :i :m
                          :u :i :m :n
                          :i :m :n :u
                          :m :n :u :i])
    (are [x] (equiv-dna x x)
      [:n] [:u] [:i] [:m])
    (are [x] (equiv-dna x x)
      [:n :n :n :n] [:u :u :u :u] [:i :i :i :i] [:m :m :m :m])
    (are [x] (equiv-dna x x)
      [:n :u :i :m] [:n :u :i :m
                     :u :i :m :n
                     :i :m :n :u
                     :m :n :u :i])
    (is (equiv-dna [:n :u :i :m] '(:n :u :i :m))))

  (testing "Non-equivalence"
    (is (not (equiv-dna [:u] [:i]))) (is (not (equiv-dna [:m] [:n])))
    (is (not (equiv-dna [:m] [:u]))) (is (not (equiv-dna [:m] [:i])))
    (is (not (equiv-dna [:n] [:u]))) (is (not (equiv-dna [:n] [:i])))
    (is (not (equiv-dna [:n] [:n :n :n :m])))
    (is (not (equiv-dna [:n :n :n :n] [:n :n :n :m])))
    (is (not (equiv-dna [:n :n :n :n :u :u :u :u :u :u :i :i :u :i :i :m]
                        [:n :u :u :u :n :u :u :i :n :u :i :i :n :u :i :n]))))
  

  (testing "Equivalence of permutation"
    (are [x] (apply equiv-dna (vals (dna-perspectives x)))
      [:m :i :u :n :m :m :u :u :m :i :m :i :m :m :m :m]
      [:m :i :u :n :i :i :n :n :u :n :u :n :n :n :n :n :m :i :u :n :m :i :u :n :u :n :u :n :u :n :u :n :m :i :u :n :i :i :n :n :m :i :u :n :i :i :n :n :m :i :u :n :m :i :u :n :m :i :u :n :m :i :u :n]))

  (testing "Equivalence of tautology reduction"
    (is (equiv-dna [:n] (repeat 4 :n) (repeat 16 :n) (repeat 64 :n)))
    (is (equiv-dna [:u] (repeat 4 :u) (repeat 16 :u) (repeat 64 :u)))
    (is (equiv-dna [:i] (repeat 4 :i) (repeat 16 :i) (repeat 64 :i)))
    (is (equiv-dna [:m] (repeat 4 :m) (repeat 16 :m) (repeat 64 :m)))
    (is (equiv-dna [:n :u :i :m]
                   [:n :u :i :m :n :u :i :m :n :u :i :m :n :u :i :m]))
    (is (equiv-dna [:n :n :n :n  :u :u :u :u  :i :i :i :i  :m :m :m :m
                    :n :n :n :n  :n :n :n :n  :i :i :i :i  :i :i :i :i
                    :n :n :n :n  :u :u :u :u  :n :n :n :n  :u :u :u :u
                    :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n]
                   [:n :u :i :m
                    :n :n :i :i
                    :n :u :n :u
                    :n :n :n :n]
                   ;; permutation:
                   [:n :u :i :m  :n :u :i :m  :n :u :i :m  :n :u :i :m
                    :n :n :i :i  :n :n :i :i  :n :n :i :i  :n :n :i :i
                    :n :u :n :u  :n :u :n :u  :n :u :n :u  :n :u :n :u
                    :n :n :n :n  :n :n :n :n  :n :n :n :n  :n :n :n :n]
                   [:n :u :i :m
                    :n :n :i :i
                    :n :u :n :u
                    :n :n :n :n]))
    (is (equiv-dna [:n :n :n :n :n :n :n :n :n :n :n :n :n :n :n :n :u :u :u :u :u :u :u :u :u :u :u :u :u :u :u :u :u :u :i :i :u :u :i :i :u :u :i :i :u :u :i :i :u :i :i :m :u :i :i :m :u :i :i :m :u :i :i :m :n :n :n :n :n :n :n :n :n :n :n :n :n :n :n :n :u :u :u :u :u :u :u :u :u :u :u :u :u :u :u :u :u :u :i :i :u :u :i :i :u :u :i :i :u :u :i :i :u :i :i :m :u :i :i :m :u :i :i :m :u :i :i :m :n :n :n :n :n :n :n :n :n :n :n :n :n :n :n :n :u :u :u :u :u :u :u :u :u :u :u :u :u :u :u :u :u :u :i :i :u :u :i :i :u :u :i :i :u :u :i :i :u :i :i :m :u :i :i :m :u :i :i :m :u :i :i :m :n :n :n :n :n :n :n :n :n :n :n :n :n :n :n :n :u :u :u :u :u :u :u :u :u :u :u :u :u :u :u :u :u :u :i :i :u :u :i :i :u :u :i :i :u :u :i :i :u :i :i :m :u :i :i :m :u :i :i :m :u :i :i :m]
                   [:n :n :n :n :u :u :u :u :u :u :i :i :u :i :i :m]
                   [:n :u :u :u :n :u :u :u :n :u :u :u :n :u :u :u :n :u :u :u :n :u :u :u :n :u :u :u :n :u :u :u :n :u :u :u :n :u :u :u :n :u :u :u :n :u :u :u :n :u :u :u :n :u :u :u :n :u :u :u :n :u :u :u :n :u :u :i :n :u :u :i :n :u :u :i :n :u :u :i :n :u :u :i :n :u :u :i :n :u :u :i :n :u :u :i :n :u :u :i :n :u :u :i :n :u :u :i :n :u :u :i :n :u :u :i :n :u :u :i :n :u :u :i :n :u :u :i :n :u :i :i :n :u :i :i :n :u :i :i :n :u :i :i :n :u :i :i :n :u :i :i :n :u :i :i :n :u :i :i :n :u :i :i :n :u :i :i :n :u :i :i :n :u :i :i :n :u :i :i :n :u :i :i :n :u :i :i :n :u :i :i :n :u :i :m :n :u :i :m :n :u :i :m :n :u :i :m :n :u :i :m :n :u :i :m :n :u :i :m :n :u :i :m :n :u :i :m :n :u :i :m :n :u :i :m :n :u :i :m :n :u :i :m :n :u :i :m :n :u :i :m :n :u :i :m]
                   [:n :u :u :u :n :u :u :i :n :u :i :i :n :u :i :m]))))



(deftest filter-dna-test
  (testing "Correctness of transformation"
    (is (= (filter-dna [:n] []) [:n]))
    (is (= (filter-dna [:i] []) [:i]))
    (is (= (filter-dna [:i :u :m :n] [:_]) [:i :u :m :n]))
    (is (= (filter-dna [:i :u :m :n] [:u]) [:u]))
    (is (= (filter-dna [:i :u :m :n] [:m]) [:n]))
    (is (= (filter-dna [:i :u :n :n  :u :i :m :u  :i :m :i :i  :m :i :u :m
                        :u :n :n :u  :i :m :u :i  :m :i :i :m  :i :u :m :i
                        :n :n :u :i  :m :u :i :m  :i :i :m :i  :u :m :i :u
                        :n :u :i :m  :u :i :m :i  :i :m :i :u  :m :i :u :n]
                       [:_ :i :_]) [:i :m :i :i
                                    :m :i :i :m
                                    :i :i :m :i
                                    :i :m :i :u]))))

(deftest dna-get-test
  ;; is just filter-dna without the vector and no holes in vpoint allowed
  (testing "IO shape"
    (is (= (dna-get [:u] [])
           :u))
    (is (= (dna-get [:i :u :u :n  :u :n :u :n  :i :m :n :m  :i :m :m :m]
                    [:m :n])
           :i))
    (is (thrown? clojure.lang.ExceptionInfo
                 (dna-get [:i :u :u :n  :u :n :u :n  :i :m :n :m  :i :m :m :m]
                          [:m :_])))))


(deftest dna-perspectives-test
  (testing "Correctness of permutations"
    (is (= (dna-perspectives (make-dna (make-dna :u :i :m :n)
                                       (make-dna :u :i :m :n)
                                       (make-dna :u :i :m :n)
                                       (make-dna :u :i :m :n)))
           {[0 1] [:u :i :m :n :u :i :m :n :u :i :m :n :u :i :m :n],
            [1 0] [:u :u :u :u :i :i :i :i :m :m :m :m :n :n :n :n]}))))


(deftest vpoint-test
  (testing "Validity of vpoint"
    (is (vpoint? []))
    (is (vpoint? [:n]))
    (is (vpoint? [:n :u]))
    (is (not (vpoint? #{:n :u})))))

(deftest rand-vpoint-test
  (testing "Validity of vpoint"
    (is (vpoint? (rand-vpoint 0)))
    (is (vpoint? (rand-vpoint 1)))
    (is (vpoint? (rand-vpoint 5)))
    (take 10 (rand-vpoint))))


(deftest vspace-test
  (testing "Validity of vspace"
    (is (vspace? [[:n] [:i] [:u] [:m]]))
    (is (vspace? #{[:n] [:i] [:u] [:m]})) ;; ? can be a set
    (is (not (vspace? [[:n :n] [:n :u] [:n :i] [:i :u]])))
    (is (vspace? (vspace nuim-code 3)))
    (is (vspace? (vspace nmui-code 3)))))


(deftest vdict-test
  (testing "Validity of vdict"
    (is (vdict? (let [vp->r {[:n :m] :m
                             [:u :u] :i
                             ;; [:x :y] :m
                             [:u :u :i] :n}]
                  (vdict {:default-result :u} vp->r))))))


(deftest dna->vdict-test
  (testing "Validity of vdict"
    (is ((every-pred vdict? sorted?)
         (dna->vdict {:sorted? true} (rand-dna 4)))))
  (testing "Correctness of transformation"
    (is (= (dna->vdict [:m :i :u :n])
           {[:n] :m, [:u] :i, [:i] :u, [:m] :n}))
    (is (= (dna->vdict [:n :m :u :u
                        :i :i :i :m
                        :m :n :u :i
                        :i :u :m :n])
           {[:n :n] :n, [:n :u] :m, [:n :i] :u, [:n :m] :u,
            [:u :n] :i, [:u :u] :i, [:u :i] :i, [:u :m] :m,
            [:i :n] :m, [:i :u] :n, [:i :i] :u, [:i :m] :i,
            [:m :n] :i, [:m :u] :u, [:m :i] :m, [:m :m] :n}))))


(deftest vdict->vmap-test
  (testing "Validity of vmap"
    (is (vmap? (vdict->vmap (dna->vdict {} [:n]))))
    (is (vmap? (vdict->vmap (dna->vdict {} [:u]))))
    (is (vmap? (vdict->vmap (dna->vdict {} [:i]))))
    (is (vmap? (vdict->vmap (dna->vdict {} [:m]))))
    (is (vmap? (vdict->vmap (dna->vdict {} (rand-dna 3))))))
  (testing "Correctness of transformation"
    (is (= :n
           (vdict->vmap {[] :n})))
    (is (= '{:n :m, :u :i, :i :u, :m :n}
           (vdict->vmap {[:n] :m, [:u] :i, [:i] :u, [:m] :n})))
    (is (= '{:n {:n :n, :u :m, :i :u, :m :u},
             :u {:n :i, :u :i, :i :i, :m :m},
             :i {:n :m, :u :n, :i :u, :m :i},
             :m {:n :i, :u :u, :i :m, :m :n}}
           (vdict->vmap {[:n :n] :n, [:n :u] :m, [:n :i] :u, [:n :m] :u,
                         [:u :n] :i, [:u :u] :i, [:u :i] :i, [:u :m] :m,
                         [:i :n] :m, [:i :u] :n, [:i :i] :u, [:i :m] :i,
                         [:m :n] :i, [:m :u] :u, [:m :i] :m, [:m :m] :n})))))

(deftest vmap-dimension-test
  (testing "Cached dimension in vmap meta"
    (is (= 5 (vmap-dimension (dna->vmap (rand-dna 5))))))
  
  (testing "Matching dimension for vmaps without meta"
    (is (= 0 (vmap-dimension :n)))
    (is (= 0 (vmap-dimension :u)))
    (is (= 1 (vmap-dimension {:n :m, :u :i, :i :u, :m :n})))
    (is (= 2 (vmap-dimension
              {:n {:n :n, :u :m, :i :u, :m :u},
               :u {:n :i, :u :i, :i :i, :m :m},
               :i {:n :m, :u :n, :i :u, :m :i},
               :m {:n :i, :u :u, :i :m, :m :n}}))))) 

(deftest vmap-perspectives-test
  (testing "Correct output shape"
    (is (= {[0 1] {:m {:m :u, :i :i, :u :m, :n :n}
                   :i {:m :u, :i :i, :u :m, :n :n}
                   :u {:m :u, :i :i, :u :m, :n :n}
                   :n {:m :u, :i :i, :u :m, :n :n}}
            [1 0] {:m {:m :u, :i :u, :u :u, :n :u}
                   :i {:m :i, :i :i, :u :i, :n :i}
                   :u {:m :m, :i :m, :u :m, :n :m}
                   :n {:m :n, :i :n, :u :n, :n :n}}}
           (vmap-perspectives
            {[0 1] [:n :m :i :u :n :m :i :u :n :m :i :u :n :m :i :u],
             [1 0] [:n :n :n :n :m :m :m :m :i :i :i :i :u :u :u :u]}))))
  (testing "Retains dna-perspectives metadata"
    (is (= '([0 1 2 3] [0 1 3 2] [0 2 1 3] [0 2 3 1] [0 3 1 2] [0 3 2 1] [1 0 2 3] [1 0 3 2] [1 2 0 3] [1 2 3 0] [1 3 0 2] [1 3 2 0] [2 0 1 3] [2 0 3 1] [2 1 0 3] [2 1 3 0] [2 3 0 1] [2 3 1 0] [3 0 1 2] [3 0 2 1] [3 1 0 2] [3 1 2 0] [3 2 0 1] [3 2 1 0])
           (:sorted-keys
            (meta (vmap-perspectives (dna-perspectives (rand-dna 4)))))))))

(deftest rel-test
  (testing "Correctness of relation"
    (is (= :n (rel) (rel :n) (rel :n :n)))
    (is (= :u (rel :u) (rel :u :u) (rel :n :u)))
    (is (= :i (rel :i) (rel :i :i) (rel :n :i)))
    (is (= :m (rel :m) (rel :m :m) (rel :n :m)
           (rel :m :n) (rel :m :u) (rel :m :i) (rel :u :i))))

  (testing "Multiple arguments"
    (is (= :m (rel :u :i :u :n))))

  (testing "formDNA relation"
    (is (= (rel [:n :m :u :i] [:m :n :i :n]) [:m :m :m :i]))
    (is (= (rel [:n :m :u :i  :i :u :n :m  :m :i :i :i  :u :u :m :n]
                [:n :u :i :m] [:n :u :i :m])
           [:n :m :u :i  :m :u :u :m  :m :i :i :i  :m :m :m :m]))
    (is (= (rel
            [:n :u :i :m  :n :n :i :i  :n :u :n :u  :n :n :n :n
             :n :u :i :m  :n :u :i :m  :n :u :n :u  :n :u :n :u
             :n :u :i :m  :n :n :i :i  :n :u :i :m  :n :n :i :i
             :n :u :i :m  :n :u :i :m  :n :u :i :m  :n :u :i :m]
            [:m :i :u :n  :i :m :n :u  :u :n :m :i  :n :u :i :m])
           [:m :m :m :m  :i :i :i :i  :u :u :u :u  :n :n :n :n
            :i :m :i :m  :m :m :m :m  :n :u :n :u  :u :u :u :u
            :u :u :m :m  :n :n :i :i  :m :m :m :m  :i :i :i :i
            :n :u :i :m  :u :u :m :m  :i :m :i :m  :m :m :m :m]))))


(deftest inv-test
  (testing "Correctness of inversion"
    (is (= :m (inv) (inv :n)))
    (is (= :i (inv :u)))
    (is (= :u (inv :i)))
    (is (= :n (inv :m))))

  (testing "formDNA inversion"
    (is (= [:m :i :u :n] (inv [:n :u :i :m])))
    (is (= [:m :n :i :u  :u :i :m :n  :n :u :u :u  :i :i :n :m]
           (inv [:n :m :u :i  :i :u :n :m  :m :i :i :i  :u :u :m :n])))))
