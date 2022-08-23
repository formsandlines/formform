(ns formform.calc-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [formform.calc :as fc]))

(deftest dna-seq-dim-test
  (testing "Dimension for sequences of 4^n elements"
    (is (== (fc/dna-seq-dim [0]) 0))
    (is (== (fc/dna-seq-dim '(0)) 0))
    (is (== (fc/dna-seq-dim (range 4)) 1))
    (is (== (fc/dna-seq-dim (range 16)) 2))
    (is (== (fc/dna-seq-dim (range 64)) 3))
    (is (== (fc/dna-seq-dim (range 256)) 4)))

  (testing "Incorrect number of elements"
    (is (nil? (fc/dna-seq-dim '())))
    (is (nil? (fc/dna-seq-dim (range 2))))
    (is (nil? (fc/dna-seq-dim (range 8)))))) 


(deftest dna-seq?-test
  (testing "Dimension validity"
    (is (fc/dna-seq? '(:N)))
    (is (not (fc/dna-seq? '(:M :N))))
    (is (fc/dna-seq? '(0)))
    (is (not (fc/dna-seq? '(0 1 2))))
    (is (fc/dna-seq? [1 0 3 2
                      3 2 0 2
                      1 2 0 1
                      0 0 1 2])))

  (testing "Non-conventional dna-seqs with valid number of distinct elements"
    (is (fc/dna-seq? '(nil)))
    (is (fc/dna-seq? '(nil nil nil nil)))
    (is (not (fc/dna-seq? '(nil nil nil nil) [nil])))
    (is (fc/dna-seq? '(nil (nil) nil (((nil))))
          [nil '(nil) '((nil)) '(((nil)))]))
    (is (fc/dna-seq? '(:x 1 a "2")))
    (is (fc/dna-seq? [:x 1 'a "2"
                      :x :x 'a 1
                      1 1 "2" "2"
                      1 :x 'a 'a]))
    (is (not (fc/dna-seq? [:x 1 'a "2"
                           :x :x 'a 1
                           2 1 "2" "2"
                           1 :x 'a 'a]))))) 


(deftest dna?-test
  (testing "Element validity"
    (is (fc/dna? :MNUI))
    (is (not (fc/dna? [:MNUI])))
    (is (not (fc/dna? [:M :N :U :I]))))) 


(deftest rand-dna-seq-test
  (testing "dna-seq validity"
    (doseq [n (range 6)]
      (is (fc/dna-seq? (fc/rand-dna-seq n))))
    (is (fc/dna-seq? (fc/rand-dna-seq 4 [0 1 2])))
    (is (fc/dna-seq? (fc/rand-dna-seq 6 [nil])))
    ;; is it confusing that elems are ignored at size > 4?
    (is (fc/dna-seq? (fc/rand-dna-seq 4 [:x :y :z :v :w]))))) 


(deftest rand-dna-test
  (testing "Validity of formDNA"
    (doseq [n (range 6)]
      (is (fc/dna? (fc/rand-dna n)))))) 

(deftest dna->digits-test
  (testing "Correctness of conversion"
    (is (= (fc/dna->digits :NUIM) '(0 1 2 3)))
    (is (= (fc/dna->digits :NUIM fc/nmui-code) '(2 3 0 1)))
    (is (= (fc/dna->digits
             :MMMMIIIIUUUUNNNNIMIMMMMMNUNUUUUUUUMMNNIIMMMMIIIINUIMUUMMIMIMMMMM
             fc/nmui-code)
           [1 1 1 1  2 0 0 2  1 3 3 1  2 2 2 2
            0 3 0 3  1 1 1 1  2 1 2 1  3 3 3 3  
            3 3 3 3  2 2 2 2  1 1 1 1  0 0 0 0 
            2 1 2 1  1 3 3 1  2 3 0 1  1 1 1 1])))) 


(deftest digits->dna-test
  (testing "Correctness of conversion"
    (is (= (fc/digits->dna [0 1 2 3]) :NUIM))
    (is (= (fc/digits->dna [1 2 0 3] fc/nmui-code) :NMUI))
    (is (= (fc/digits->dna [1 0 3 2  0 1 2 3  3 2 1 0  2 3 0 1] fc/nmui-code)
           :MIUNIMNUUNMINUIM))
    (is (= (fc/digits->dna [2 3 0 1  2 0 0 2  2 3 0 1  2 0 0 2 
                            0 3 0 3  2 3 0 1  2 3 0 1  0 3 0 3 
                            0 3 0 3  2 0 0 2  2 3 0 1  0 0 0 0 
                            2 3 0 1  2 3 0 1  2 3 0 1  2 3 0 1] fc/nmui-code)
           :NUIMNNIINUNUNNNNNUIMNUIMNUNUNUNUNUIMNNIINUIMNNIINUIMNUIMNUIMNUIM)))) 


(deftest reorder-dna-seq-test
  (testing "Correctness of reordered dna-seq"
    (is (= (fc/reorder-dna-seq
             [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N]
             fc/nuim-code fc/nmui-code)
           [:I :I :I :I :U :U :U :U :M :M :M :M :N :N :N :N]))
    (is (= (fc/reorder-dna-seq
             [:I :I :I :I :U :U :U :U :M :M :M :M :N :N :N :N]
             fc/nmui-code fc/nuim-code)
           [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N])))) 


(deftest compare-dna-test
  (testing "Correctness of sorted order"
    (is (= (sort fc/compare-dna [:NMUI :NUIM :M])
           [:M :NUIM :NMUI]))
    (is (= (sort fc/compare-dna [[:NUIM :IM] [:UMI :MNU]])
           [[:UMI :MNU] [:NUIM :IM]])))) 


(deftest expand-dna-seq-test
  (testing "Correctness of expansion"
    (is (= (fc/expand-dna-seq [:M :I :U :N] 2)
           [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N])))) 


(deftest dna->quaternary-test
  (testing "Correctness of conversion"
    (is (= "4r0123" (fc/dna->quaternary :NUIM)))
    (is (= "4r0312" (fc/dna->quaternary :NMUI)))
    ;; should this return nil?
    ; (is (= "4r23" (fc/dna->quaternary :IM)))
    )) 


(deftest permute-dna-seq-test
  (testing "Correctness of permutation"
    (is (= (fc/permute-dna-seq
             [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N] [1 0])
           [[1 0] [:M :I :U :N :M :I :U :N :M :I :U :N :M :I :U :N]])))) 


(deftest dna-seq-perspectives-test
  (testing "Correctness of permutations"
    (is (= (fc/dna-seq-perspectives
             [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N])
           [[[0 1] [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N]]
            [[1 0] [:M :I :U :N :M :I :U :N :M :I :U :N :M :I :U :N]]])))) 


(deftest rand-vpoint-test
  (testing "Validity of vpoint"
    (is (fc/vpoint? (fc/rand-vpoint 5))))) 


(deftest vspace-test
  (testing "Validity of vspace"
    (is (fc/vspace? (fc/vspace 3 fc/nuim-code)))
    (is (fc/vspace? (fc/vspace 3 fc/nmui-code))))) 


(deftest vdict-test
  (testing "Validity of vdict"
    (is (fc/vdict? (let [vp->r {[:N :M] :M
                                [:U :U] :I
                                [:X :Y] :M
                                [:U :U :I] :N}]
                     (fc/vdict vp->r {:default-result :U}))))) )


(deftest dna->vdict-test
  (testing "Validity of vdict"
    (is (fc/vdict? (fc/dna->vdict (fc/rand-dna 4) {:sorted? true}))))) 


(deftest vdict->vmap-test
  (testing "Validity of vmap"
    (is (fc/vmap? (fc/vdict->vmap (fc/dna->vdict (fc/rand-dna 3) {})))))) 


(deftest rel-test
  (testing "Correctness of relation"
    (is (= :N (fc/rel) (fc/rel :N) (fc/rel :N :N)))
    (is (= :U (fc/rel :U) (fc/rel :U :U) (fc/rel :N :U)))
    (is (= :I (fc/rel :I) (fc/rel :I :I) (fc/rel :N :I)))
    (is (= :M (fc/rel :M) (fc/rel :M :M) (fc/rel :N :M)
           (fc/rel :M :N) (fc/rel :M :U) (fc/rel :M :I) (fc/rel :U :I))))

  (testing "Multiple arguments"
    (is (= :M (fc/rel :U :I :U :N))))

  (testing "formDNA relation"
    (is (= (fc/rel :NMUI :MNIN) :MMMI))
    (is (= (fc/rel :NMUIIUNMMIIIUUMN :NUIM :NUIM) :NMUIMUUMMIIIMMMM))
    (is (= (fc/rel
             :NUIMNNIINUNUNNNNNUIMNUIMNUNUNUNUNUIMNNIINUIMNNIINUIMNUIMNUIMNUIM
             :MIUNIMNUUNMINUIM) 
           :MMMMIIIIUUUUNNNNIMIMMMMMNUNUUUUUUUMMNNIIMMMMIIIINUIMUUMMIMIMMMMM)))) 


(deftest inv-test
  (testing "Correctness of inversion"
    (is (= :M (fc/inv) (fc/inv :N)))
    (is (= :I (fc/inv :U)))
    (is (= :U (fc/inv :I)))
    (is (= :N (fc/inv :M))))

  (testing "formDNA inversion"
    (is (= :MIUN (fc/inv :NUIM)))
    (is (= :MNIUUIMNNUUUIINM (fc/inv :NMUIIUNMMIIIUUMN))))) 



