(ns formform.calc-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [formform.calc :as calc :refer :all]))

(deftest dna-seq-dim-test
  (testing "Dimension for sequences of 4^n elements"
    (is (== (dna-seq-dim [0]) 0))
    (is (== (dna-seq-dim '(0)) 0))
    (is (== (dna-seq-dim (range 4)) 1))
    (is (== (dna-seq-dim (range 16)) 2))
    (is (== (dna-seq-dim (range 64)) 3))
    (is (== (dna-seq-dim (range 256)) 4)))

  (testing "Incorrect number of elements"
    (is (nil? (dna-seq-dim '())))
    (is (nil? (dna-seq-dim (range 2))))
    (is (nil? (dna-seq-dim (range 8)))))) 


(deftest dna-seq?-test
  (testing "Dimension validity"
    (is (dna-seq? '(:N)))
    (is (not (dna-seq? '(:M :N))))
    (is (dna-seq? '(0)))
    (is (not (dna-seq? '(0 1 2))))
    (is (dna-seq? [1 0 3 2
                      3 2 0 2
                      1 2 0 1
                      0 0 1 2])))

  (testing "Non-conventional dna-seqs with valid number of distinct elements"
    (is (dna-seq? '(nil)))
    (is (dna-seq? '(nil nil nil nil)))
    (is (not (dna-seq? '(nil nil nil nil) [nil])))
    (is (dna-seq? '(nil (nil) nil (((nil))))
          [nil '(nil) '((nil)) '(((nil)))]))
    (is (dna-seq? '(:x 1 a "2")))
    (is (dna-seq? [:x 1 'a "2"
                      :x :x 'a 1
                      1 1 "2" "2"
                      1 :x 'a 'a]))
    (is (not (dna-seq? [:x 1 'a "2"
                           :x :x 'a 1
                           2 1 "2" "2"
                           1 :x 'a 'a]))))) 


(deftest dna?-test
  (testing "Element validity"
    (is (dna? :MNUI))
    (is (not (dna? [:MNUI])))
    (is (not (dna? [:M :N :U :I]))))) 


(deftest rand-dna-seq-test
  (testing "dna-seq validity"
    (doseq [n (range 6)]
      (is (dna-seq? (rand-dna-seq n))))
    (is (dna-seq? (rand-dna-seq 4 [0 1 2])))
    (is (dna-seq? (rand-dna-seq 6 [nil])))
    ;; is it confusing that elems are ignored at size > 4?
    (is (dna-seq? (rand-dna-seq 4 [:x :y :z :v :w]))))) 


(deftest rand-dna-test
  (testing "Validity of formDNA"
    (doseq [n (range 6)]
      (is (dna? (rand-dna n)))))) 

(deftest dna->digits-test
  (testing "Correctness of conversion"
    (is (= (dna->digits :NUIM) '(0 1 2 3)))
    (is (= (dna->digits :NUIM nmui-code) '(2 3 0 1)))
    (is (= (dna->digits
             :MMMMIIIIUUUUNNNNIMIMMMMMNUNUUUUUUUMMNNIIMMMMIIIINUIMUUMMIMIMMMMM
             nmui-code)
           [1 1 1 1  2 0 0 2  1 3 3 1  2 2 2 2
            0 3 0 3  1 1 1 1  2 1 2 1  3 3 3 3  
            3 3 3 3  2 2 2 2  1 1 1 1  0 0 0 0 
            2 1 2 1  1 3 3 1  2 3 0 1  1 1 1 1])))) 


(deftest digits->dna-test
  (testing "Correctness of conversion"
    (is (= (digits->dna [0 1 2 3]) :NUIM))
    (is (= (digits->dna [1 2 0 3] nmui-code) :NMUI))
    (is (= (digits->dna [1 0 3 2  0 1 2 3  3 2 1 0  2 3 0 1] nmui-code)
           :MIUNIMNUUNMINUIM))
    (is (= (digits->dna [2 3 0 1  2 0 0 2  2 3 0 1  2 0 0 2 
                            0 3 0 3  2 3 0 1  2 3 0 1  0 3 0 3 
                            0 3 0 3  2 0 0 2  2 3 0 1  0 0 0 0 
                            2 3 0 1  2 3 0 1  2 3 0 1  2 3 0 1] nmui-code)
           :NUIMNNIINUNUNNNNNUIMNUIMNUNUNUNUNUIMNNIINUIMNNIINUIMNUIMNUIMNUIM)))) 


(deftest reorder-dna-seq-test
  (testing "Correctness of reordered dna-seq"
    (is (= (reorder-dna-seq
             [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N]
             nuim-code nmui-code)
           [:I :I :I :I :U :U :U :U :M :M :M :M :N :N :N :N]))
    (is (= (reorder-dna-seq
             [:I :I :I :I :U :U :U :U :M :M :M :M :N :N :N :N]
             nmui-code nuim-code)
           [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N])))) 


(deftest compare-dna-test
  (testing "Correctness of sorted order"
    (is (= (sort compare-dna [:NMUI :NUIM :M])
           [:M :NUIM :NMUI]))
    (is (= (sort compare-dna [[:NUIM :IM] [:UMI :MNU]])
           [[:UMI :MNU] [:NUIM :IM]])))) 


(deftest expand-dna-seq-test
  (testing "Correctness of expansion"
    (is (= (expand-dna-seq [:M :I :U :N] 2)
           [:M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N]))
    (is (= (expand-dna-seq [:N :U :I :M
                               :U :I :M :I
                               :I :M :I :U
                               :M :I :U :N] 3)
           [:N :N :N :N :U :U :U :U :I :I :I :I :M :M :M :M 
            :U :U :U :U :I :I :I :I :M :M :M :M :I :I :I :I 
            :I :I :I :I :M :M :M :M :I :I :I :I :U :U :U :U 
            :M :M :M :M :I :I :I :I :U :U :U :U :N :N :N :N])))) 

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
        (is (= (filter-dna :NMUI [:U]) :U))
        (is (= (filter-dna :NMUI [:M]) :N))
        (is (= (filter-dna :NUIMUIMIIMIUMIUNUIMUIMIIMIUMIUNNIMUIMIIMIUMIUNNUMUIMIIMIUMIUNNUI [:_ :I :_]) :UIMIIMIIMIIMIIMI)))) 


(deftest dna->quaternary-test
  (testing "Correctness of conversion"
    (is (= "4r0123" (dna->quaternary :NUIM)))
    (is (= "4r0312" (dna->quaternary :NMUI)))
    ;; should this return nil?
    ; (is (= "4r23" (dna->quaternary :IM)))
    )) 


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


(deftest rand-vpoint-test
  (testing "Validity of vpoint"
    (is (vpoint? (rand-vpoint 5))))) 


(deftest vspace-test
  (testing "Validity of vspace"
    (is (vspace? (vspace 3 nuim-code)))
    (is (vspace? (vspace 3 nmui-code))))) 


(deftest vdict-test
  (testing "Validity of vdict"
    (is (vdict? (let [vp->r {[:N :M] :M
                                [:U :U] :I
                                [:X :Y] :M
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
    (is (= (rel :NMUI :MNIN) :MMMI))
    (is (= (rel :NMUIIUNMMIIIUUMN :NUIM :NUIM) :NMUIMUUMMIIIMMMM))
    (is (= (rel
             :NUIMNNIINUNUNNNNNUIMNUIMNUNUNUNUNUIMNNIINUIMNNIINUIMNUIMNUIMNUIM
             :MIUNIMNUUNMINUIM) 
           :MMMMIIIIUUUUNNNNIMIMMMMMNUNUUUUUUUMMNNIIMMMMIIIINUIMUUMMIMIMMMMM)))) 


(deftest inv-test
  (testing "Correctness of inversion"
    (is (= :M (inv) (inv :N)))
    (is (= :I (inv :U)))
    (is (= :U (inv :I)))
    (is (= :N (inv :M))))

  (testing "formDNA inversion"
    (is (= :MIUN (inv :NUIM)))
    (is (= :MNIUUIMNNUUUIINM (inv :NMUIIUNMMIIIUUMN))))) 



