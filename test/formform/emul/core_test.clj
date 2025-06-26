(ns formform.emul.core-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [formform.calc.specs :refer [fns-with-specs]]
            [formform.emul.core :refer :all]
            [orchestra.spec.test :as stest]))

;; (doseq [fsym fns-with-specs] (stest/instrument fsym))

#_
(deftest sys-ini-test
  (testing "Correct output format"
    (is (= 24 (count (sys-ini :random {:w 4 :h 6}))))
    (is (= 2400 (count (sys-ini :square {:w 40 :h 60}))))))



(deftest segment-bounds-test
  (testing "Correct segment boundaries"
    ;; [0 1 2 3 4 5 6 7 8]
    ;;  | anchor (start)
    ;;                    | anchor (end)
    ;; [0 1 2 3]           ;; start/end start
    ;;  2 3]         [0 1  ;; start/end center
    ;;           [0 1 2 3] ;; start/end end
    
    ;; [0 1 2 3 4 5 6 7 8]
    ;;          | anchor
    ;;         [0 1 2 3]   ;; center start
    ;;     [0 1 2 3]       ;; center center
    ;; [0 1 2 3]           ;; center end
    (is (= (let [opts [:start :center :end]]
             (for [align opts
                   self-align opts]
               [[align self-align] (segment-bounds 9 4 align self-align 0)]))
           [[[:start :start] [0 3]]
            [[:start :center] [7 1]]
            [[:start :end] [5 8]]
            [[:center :start] [4 7]]
            [[:center :center] [2 5]]
            [[:center :end] [0 3]]
            [[:end :start] [0 3]]
            [[:end :center] [7 1]]
            [[:end :end] [5 8]]]))

    ;; [0 1 2 3 4 5 6 7 8]
    ;;  | anchor (start)
    ;;                    | anchor (end)
    ;; [0 1 2 3 4]         ;; start/end start
    ;;  2 3 4]       [0 1  ;; start/end center
    ;;         [0 1 2 3 4] ;; start/end end
    
    ;; [0 1 2 3 4 5 6 7 8]
    ;;          | anchor
    ;;         [0 1 2 3 4]   ;; center start
    ;;     [0 1 2 3 4]       ;; center center
    ;;  1 2 3 4]       [0    ;; center end
    (is (= (let [opts [:start :center :end]]
             (for [align opts
                   self-align opts]
               [[align self-align] (segment-bounds 9 5 align self-align 0)]))
           [[[:start :start] [0 4]]
            [[:start :center] [7 2]]
            [[:start :end] [4 8]]
            [[:center :start] [4 8]]
            [[:center :center] [2 6]]
            [[:center :end] [8 3]]
            [[:end :start] [0 4]]
            [[:end :center] [7 2]]
            [[:end :end] [4 8]]]))


    ;; [0 1 2 3 4 5 6 7 8 9]
    ;;  | anchor (start)
    ;;            | anchor (end)
    ;; [0 1 2 3]             ;; start/end start
    ;;  2 3]           [0 1  ;; start/end center
    ;;             [0 1 2 3] ;; start/end end

    ;; [0 1 2 3 4 5 6 7 8 9]
    ;;            | anchor
    ;;           [0 1 2 3]   ;; center start
    ;;       [0 1 2 3]       ;; center center
    ;;   [0 1 2 3]           ;; center end
    (is (= (let [opts [:start :center :end]]
             (for [align opts
                   self-align opts]
               [[align self-align] (segment-bounds 10 4 align self-align 0)]))
           [[[:start :start] [0 3]]
            [[:start :center] [8 1]]
            [[:start :end] [6 9]]
            [[:center :start] [5 8]]
            [[:center :center] [3 6]]
            [[:center :end] [1 4]]
            [[:end :start] [0 3]]
            [[:end :center] [8 1]]
            [[:end :end] [6 9]]]))

    ;; [0 1 2]
    ;;  | anchor (start)
    ;;        | anchor (end)
    ;; [0 1]   ;; start/end start
    ;;  1] [0  ;; start/end center
    ;;   [0 1] ;; start/end end

    ;; [0 1 2]
    ;;    | anchor
    ;;   [0 1] ;; center start
    ;; [0 1]   ;; center center
    ;;  1] [0  ;; center end
    (is (= (let [opts [:start :center :end]]
             (for [align opts
                   self-align opts]
               [[align self-align] (segment-bounds 3 2 align self-align 0)]))
           [[[:start :start] [0 1]]
            [[:start :center] [2 0]]
            [[:start :end] [1 2]]
            [[:center :start] [1 2]]
            [[:center :center] [0 1]]
            [[:center :end] [2 0]]
            [[:end :start] [0 1]]
            [[:end :center] [2 0]]
            [[:end :end] [1 2]]]))

    ;; [0 1]
    ;;  | anchor (start)
    ;;      | anchor (end)
    ;; [0 1] ;; start/end start
    ;;  1|0  ;; start/end center
    ;; [0 1] ;; start/end end

    ;; [0 1]
    ;;    | anchor
    ;;  1|0  ;; center start
    ;; [0 1] ;; center center
    ;;  1|0  ;; center end
    (is (= (let [opts [:start :center :end]]
             (for [align opts
                   self-align opts]
               [[align self-align] (segment-bounds 2 2 align self-align 0)]))
           [[[:start :start] [0 1]]
            [[:start :center] [1 0]]
            [[:start :end] [0 1]]
            [[:center :start] [1 0]]
            [[:center :center] [0 1]]
            [[:center :end] [1 0]]
            [[:end :start] [0 1]]
            [[:end :center] [1 0]]
            [[:end :end] [0 1]]]))

    ;; [0 1]
    ;;  | anchor (start)
    ;;      | anchor (end)
    ;; [0]   ;; start/end start
    ;; [0]   ;; start/end center
    ;;   [0] ;; start/end end

    ;; [0 1]
    ;;    | anchor
    ;;   [0] ;; center start
    ;;   [0] ;; center center
    ;; [0]   ;; center end
    (is (= (let [opts [:start :center :end]]
             (for [align opts
                   self-align opts]
               [[align self-align] (segment-bounds 2 1 align self-align 0)]))
           [[[:start :start] [0 0]]
            [[:start :center] [0 0]]
            [[:start :end] [1 1]]
            [[:center :start] [1 1]]
            [[:center :center] [1 1]]
            [[:center :end] [0 0]]
            [[:end :start] [0 0]]
            [[:end :center] [0 0]]
            [[:end :end] [1 1]]]))

    ;; [0]  trivial case
    (is (= (let [opts [:start :center :end]]
             (for [align opts
                   self-align opts]
               (segment-bounds 1 1 align self-align 0)))
           [[0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0]]))))




