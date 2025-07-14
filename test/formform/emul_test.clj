(ns formform.emul-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [clojure.edn :as edn]
            [formform.emul :refer :all]
            [formform.emul.interfaces :as i]
            [formform.calc :as calc]
            [formform.utils :as utils]
            [orchestra.spec.test :as stest]))

(doseq [fsym fns-with-specs] (stest/instrument fsym))

;; Ini tests

(deftest ini-constant-test
  (testing "constant value"
    (is (= (sys-ini (make-ini :constant :u) [10])
           (repeat 10 :u)))
    (is (= (sys-ini (make-ini :constant :i) [5 5])
           (repeat 5 (repeat 5 :i))))))

(deftest ini-random-test
  (testing "random variance"
    (is (not= (sys-ini (make-ini :random) [10])
              (sys-ini (make-ini :random) [10])
              (sys-ini (make-ini :random) [10])))
    (is (not= (sys-ini (make-ini :random) [5 5])
              (sys-ini (make-ini :random) [5 5]))))
  (testing "random reproducability"
    (is (= (sys-ini (make-ini :random) [10] {:seed 42})
           (sys-ini (make-ini :random) [10] {:seed 42})
           (sys-ini (make-ini :random) [10] {:seed 42})))
    (is (= (sys-ini (make-ini :random) [5 5] {:seed 69})
           (sys-ini (make-ini :random) [5 5] {:seed 69}))))
  (testing "weights approximation in random generation"
    (is (= (let [w [0.1 0.2 0.3 0.4]
                 freqs (->> (sys-ini (make-ini :random {:weights w}) [10000])
                            (frequencies))
                 sum (+ 0.0 (reduce + (vals freqs)))]
             (update-vals freqs #(Math/round (* 10 (/ % sum)))))
           {:n 1 :u 2 :i 3 :m 4}))))

(deftest ini-cycle-test
  (testing "cycling sequence and direction"
    (is (= (sys-ini (make-ini :cycle [:m :i :u]) [10])
           [:m :i :u :m :i :u :m :i :u :m]))
    (is (= (sys-ini (make-ini :cycle [:u :m :i :n]) [3 3])
           ;; cycles x and y directions, not linewise!
           [[:u :m :i]
            [:m :i :n]
            [:i :n :u]]))))


#_
(comment
    (def alignments [:topleft :topcenter :topright
                     :left :center :right
                     :bottomleft :bottomcenter :bottomright])
    (for [pos   alignments
          align alignments]
      [pos align
       (sys-ini (make-ini :figure :n [[:u :i :m]
                                      [:i :m :u]
                                      [:m :u :i]]
                          {:pos pos :align align})
                [5 5])])
    ,)

(deftest ini-figure-test
  (testing "correct 1D figure position and alignment"
    (is (= (sys-ini (make-ini :figure :n [:u :i :m]
                              {:pos :left :align :left}) [9])
           [:u :i :m :n :n :n :n :n :n]))
    (is (= (sys-ini (make-ini :figure :n [:u :i :m]
                              {:pos :center :align :left}) [9])
           [:n :n :n :n :u :i :m :n :n]))
    (is (= (sys-ini (make-ini :figure :n [:u :i :m]
                              {:pos :right :align :left}) [9])
           [:u :i :m :n :n :n :n :n :n]))
    (is (= (sys-ini (make-ini :figure :n [:u :i :m]
                              {:pos :left :align :center}) [9])
           [:i :m :n :n :n :n :n :n :u]))
    (is (= (sys-ini (make-ini :figure :n [:u :i :m]
                              {:pos :center :align :center}) [9])
           [:n :n :n :u :i :m :n :n :n]))
    (is (= (sys-ini (make-ini :figure :n [:u :i :m]
                              {:pos :right :align :center}) [9])
           [:i :m :n :n :n :n :n :n :u]))
    (is (= (sys-ini (make-ini :figure :n [:u :i :m]
                              {:pos :left :align :right}) [9])
           [:n :n :n :n :n :n :u :i :m]))
    (is (= (sys-ini (make-ini :figure :n [:u :i :m]
                              {:pos :center :align :right}) [9])
           [:n :u :i :m :n :n :n :n :n]))
    (is (= (sys-ini (make-ini :figure :n [:u :i :m]
                              {:pos :right :align :right}) [9])
           [:n :n :n :n :n :n :u :i :m])))
  (testing "correct 2D figure position and alignment"
    ;; all verified
    (are [pos align gen] (= (sys-ini (make-ini :figure :n [[:u :i :m]
                                                           [:i :m :u]
                                                           [:m :u :i]]
                                               {:pos pos :align align})
                                     [5 5]) gen)
      :topleft :topleft
      [[:u :i :m :n :n]
       [:i :m :u :n :n]
       [:m :u :i :n :n]
       [:n :n :n :n :n]
       [:n :n :n :n :n]]

      :topleft :topcenter
      [[:i :m :n :n :u]
       [:m :u :n :n :i]
       [:u :i :n :n :m]
       [:n :n :n :n :n]
       [:n :n :n :n :n]]

      :topleft :topright
      [[:n :n :u :i :m]
       [:n :n :i :m :u]
       [:n :n :m :u :i]
       [:n :n :n :n :n]
       [:n :n :n :n :n]]

      :topleft :left
      [[:i :m :u :n :n]
       [:m :u :i :n :n]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:u :i :m :n :n]]

      :topleft :center
      [[:m :u :n :n :i]
       [:u :i :n :n :m]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:i :m :n :n :u]]

      :topleft :right
      [[:n :n :i :m :u]
       [:n :n :m :u :i]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :n :u :i :m]]

      :topleft :bottomleft
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:u :i :m :n :n]
       [:i :m :u :n :n]
       [:m :u :i :n :n]]

      :topleft :bottomcenter
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:i :m :n :n :u]
       [:m :u :n :n :i]
       [:u :i :n :n :m]]

      :topleft :bottomright
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :n :u :i :m]
       [:n :n :i :m :u]
       [:n :n :m :u :i]]

      :topcenter :topleft
      [[:n :n :u :i :m]
       [:n :n :i :m :u]
       [:n :n :m :u :i]
       [:n :n :n :n :n]
       [:n :n :n :n :n]]

      :topcenter :topcenter
      [[:n :u :i :m :n]
       [:n :i :m :u :n]
       [:n :m :u :i :n]
       [:n :n :n :n :n]
       [:n :n :n :n :n]]

      :topcenter :topright
      [[:i :m :n :n :u]
       [:m :u :n :n :i]
       [:u :i :n :n :m]
       [:n :n :n :n :n]
       [:n :n :n :n :n]]

      :topcenter :left
      [[:n :n :i :m :u]
       [:n :n :m :u :i]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :n :u :i :m]]

      :topcenter :center
      [[:n :i :m :u :n]
       [:n :m :u :i :n]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :u :i :m :n]]

      :topcenter :right
      [[:m :u :n :n :i]
       [:u :i :n :n :m]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:i :m :n :n :u]]

      :topcenter :bottomleft
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :n :u :i :m]
       [:n :n :i :m :u]
       [:n :n :m :u :i]]

      :topcenter :bottomcenter
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :u :i :m :n]
       [:n :i :m :u :n]
       [:n :m :u :i :n]]

      :topcenter :bottomright
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:i :m :n :n :u]
       [:m :u :n :n :i]
       [:u :i :n :n :m]]

      :topright :topleft
      [[:u :i :m :n :n]
       [:i :m :u :n :n]
       [:m :u :i :n :n]
       [:n :n :n :n :n]
       [:n :n :n :n :n]]

      :topright :topcenter
      [[:i :m :n :n :u]
       [:m :u :n :n :i]
       [:u :i :n :n :m]
       [:n :n :n :n :n]
       [:n :n :n :n :n]]

      :topright :topright
      [[:n :n :u :i :m]
       [:n :n :i :m :u]
       [:n :n :m :u :i]
       [:n :n :n :n :n]
       [:n :n :n :n :n]]

      :topright :left
      [[:i :m :u :n :n]
       [:m :u :i :n :n]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:u :i :m :n :n]]

      :topright :center
      [[:m :u :n :n :i]
       [:u :i :n :n :m]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:i :m :n :n :u]]

      :topright :right
      [[:n :n :i :m :u]
       [:n :n :m :u :i]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :n :u :i :m]]

      :topright :bottomleft
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:u :i :m :n :n]
       [:i :m :u :n :n]
       [:m :u :i :n :n]]

      :topright :bottomcenter
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:i :m :n :n :u]
       [:m :u :n :n :i]
       [:u :i :n :n :m]]

      :topright :bottomright
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :n :u :i :m]
       [:n :n :i :m :u]
       [:n :n :m :u :i]]

      :left :topleft
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:u :i :m :n :n]
       [:i :m :u :n :n]
       [:m :u :i :n :n]]

      :left :topcenter
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:i :m :n :n :u]
       [:m :u :n :n :i]
       [:u :i :n :n :m]]

      :left :topright
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :n :u :i :m]
       [:n :n :i :m :u]
       [:n :n :m :u :i]]

      :left :left
      [[:n :n :n :n :n]
       [:u :i :m :n :n]
       [:i :m :u :n :n]
       [:m :u :i :n :n]
       [:n :n :n :n :n]]

      :left :center
      [[:n :n :n :n :n]
       [:i :m :n :n :u]
       [:m :u :n :n :i]
       [:u :i :n :n :m]
       [:n :n :n :n :n]]

      :left :right
      [[:n :n :n :n :n]
       [:n :n :u :i :m]
       [:n :n :i :m :u]
       [:n :n :m :u :i]
       [:n :n :n :n :n]]

      :left :bottomleft
      [[:i :m :u :n :n]
       [:m :u :i :n :n]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:u :i :m :n :n]]

      :left :bottomcenter
      [[:m :u :n :n :i]
       [:u :i :n :n :m]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:i :m :n :n :u]]

      :left :bottomright
      [[:n :n :i :m :u]
       [:n :n :m :u :i]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :n :u :i :m]]

      :center :topleft
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :n :u :i :m]
       [:n :n :i :m :u]
       [:n :n :m :u :i]]

      :center :topcenter
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :u :i :m :n]
       [:n :i :m :u :n]
       [:n :m :u :i :n]]

      :center :topright
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:i :m :n :n :u]
       [:m :u :n :n :i]
       [:u :i :n :n :m]]

      :center :left
      [[:n :n :n :n :n]
       [:n :n :u :i :m]
       [:n :n :i :m :u]
       [:n :n :m :u :i]
       [:n :n :n :n :n]]

      :center :center
      [[:n :n :n :n :n]
       [:n :u :i :m :n]
       [:n :i :m :u :n]
       [:n :m :u :i :n]
       [:n :n :n :n :n]]

      :center :right
      [[:n :n :n :n :n]
       [:i :m :n :n :u]
       [:m :u :n :n :i]
       [:u :i :n :n :m]
       [:n :n :n :n :n]]

      :center :bottomleft
      [[:n :n :i :m :u]
       [:n :n :m :u :i]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :n :u :i :m]]

      :center :bottomcenter
      [[:n :i :m :u :n]
       [:n :m :u :i :n]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :u :i :m :n]]

      :center :bottomright
      [[:m :u :n :n :i]
       [:u :i :n :n :m]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:i :m :n :n :u]]

      :right :topleft
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:u :i :m :n :n]
       [:i :m :u :n :n]
       [:m :u :i :n :n]]

      :right :topcenter
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:i :m :n :n :u]
       [:m :u :n :n :i]
       [:u :i :n :n :m]]

      :right :topright
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :n :u :i :m]
       [:n :n :i :m :u]
       [:n :n :m :u :i]]

      :right :left
      [[:n :n :n :n :n]
       [:u :i :m :n :n]
       [:i :m :u :n :n]
       [:m :u :i :n :n]
       [:n :n :n :n :n]]

      :right :center
      [[:n :n :n :n :n]
       [:i :m :n :n :u]
       [:m :u :n :n :i]
       [:u :i :n :n :m]
       [:n :n :n :n :n]]

      :right :right
      [[:n :n :n :n :n]
       [:n :n :u :i :m]
       [:n :n :i :m :u]
       [:n :n :m :u :i]
       [:n :n :n :n :n]]

      :right :bottomleft
      [[:i :m :u :n :n]
       [:m :u :i :n :n]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:u :i :m :n :n]]

      :right :bottomcenter
      [[:m :u :n :n :i]
       [:u :i :n :n :m]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:i :m :n :n :u]]

      :right :bottomright
      [[:n :n :i :m :u]
       [:n :n :m :u :i]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :n :u :i :m]]

      :bottomleft :topleft
      [[:u :i :m :n :n]
       [:i :m :u :n :n]
       [:m :u :i :n :n]
       [:n :n :n :n :n]
       [:n :n :n :n :n]]

      :bottomleft :topcenter
      [[:i :m :n :n :u]
       [:m :u :n :n :i]
       [:u :i :n :n :m]
       [:n :n :n :n :n]
       [:n :n :n :n :n]]

      :bottomleft :topright
      [[:n :n :u :i :m]
       [:n :n :i :m :u]
       [:n :n :m :u :i]
       [:n :n :n :n :n]
       [:n :n :n :n :n]]

      :bottomleft :left
      [[:i :m :u :n :n]
       [:m :u :i :n :n]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:u :i :m :n :n]]

      :bottomleft :center
      [[:m :u :n :n :i]
       [:u :i :n :n :m]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:i :m :n :n :u]]

      :bottomleft :right
      [[:n :n :i :m :u]
       [:n :n :m :u :i]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :n :u :i :m]]

      :bottomleft :bottomleft
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:u :i :m :n :n]
       [:i :m :u :n :n]
       [:m :u :i :n :n]]

      :bottomleft :bottomcenter
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:i :m :n :n :u]
       [:m :u :n :n :i]
       [:u :i :n :n :m]]

      :bottomleft :bottomright
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :n :u :i :m]
       [:n :n :i :m :u]
       [:n :n :m :u :i]]

      :bottomcenter :topleft
      [[:n :n :u :i :m]
       [:n :n :i :m :u]
       [:n :n :m :u :i]
       [:n :n :n :n :n]
       [:n :n :n :n :n]]

      :bottomcenter :topcenter
      [[:n :u :i :m :n]
       [:n :i :m :u :n]
       [:n :m :u :i :n]
       [:n :n :n :n :n]
       [:n :n :n :n :n]]

      :bottomcenter :topright
      [[:i :m :n :n :u]
       [:m :u :n :n :i]
       [:u :i :n :n :m]
       [:n :n :n :n :n]
       [:n :n :n :n :n]]

      :bottomcenter :left
      [[:n :n :i :m :u]
       [:n :n :m :u :i]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :n :u :i :m]]

      :bottomcenter :center
      [[:n :i :m :u :n]
       [:n :m :u :i :n]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :u :i :m :n]]

      :bottomcenter :right
      [[:m :u :n :n :i]
       [:u :i :n :n :m]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:i :m :n :n :u]]

      :bottomcenter :bottomleft
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :n :u :i :m]
       [:n :n :i :m :u]
       [:n :n :m :u :i]]

      :bottomcenter :bottomcenter
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :u :i :m :n]
       [:n :i :m :u :n]
       [:n :m :u :i :n]]

      :bottomcenter :bottomright
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:i :m :n :n :u]
       [:m :u :n :n :i]
       [:u :i :n :n :m]]

      :bottomright :topleft
      [[:u :i :m :n :n]
       [:i :m :u :n :n]
       [:m :u :i :n :n]
       [:n :n :n :n :n]
       [:n :n :n :n :n]]

      :bottomright :topcenter
      [[:i :m :n :n :u]
       [:m :u :n :n :i]
       [:u :i :n :n :m]
       [:n :n :n :n :n]
       [:n :n :n :n :n]]

      :bottomright :topright
      [[:n :n :u :i :m]
       [:n :n :i :m :u]
       [:n :n :m :u :i]
       [:n :n :n :n :n]
       [:n :n :n :n :n]]

      :bottomright :left
      [[:i :m :u :n :n]
       [:m :u :i :n :n]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:u :i :m :n :n]]

      :bottomright :center
      [[:m :u :n :n :i]
       [:u :i :n :n :m]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:i :m :n :n :u]]

      :bottomright :right
      [[:n :n :i :m :u]
       [:n :n :m :u :i]
       [:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :n :u :i :m]]

      :bottomright :bottomleft
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:u :i :m :n :n]
       [:i :m :u :n :n]
       [:m :u :i :n :n]]

      :bottomright :bottomcenter
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:i :m :n :n :u]
       [:m :u :n :n :i]
       [:u :i :n :n :m]]

      :bottomright :bottomright
      [[:n :n :n :n :n]
       [:n :n :n :n :n]
       [:n :n :u :i :m]
       [:n :n :i :m :u]
       [:n :n :m :u :i]]))

  (testing "1D figure in 2D generation"
    (is (= (sys-ini (make-ini :figure :n (ini-patterns :ball)
                              :center)
                    [7 5])
           [[:n :n :n :n :n :n :n]
            [:n :n :n :n :n :n :n]
            [:n :i :u :m :u :i :n]
            [:n :n :n :n :n :n :n]
            [:n :n :n :n :n :n :n]])))
  (testing "background ini & pattern holes"
    (is (= (sys-ini (make-ini :figure (make-ini :cycle [:i :u])
                              [[:n :m] [:m :_]]
                              {:pos :left :align :left}) [3 3])
           [[:n :m :i]
            [:m :i :u]
            [:i :u :i]])))
  (testing "pattern function"
    (is (= (sys-ini (make-ini :figure
                              :n {:w 3 :h 2
                                  :f (fn [{:keys [x y] :as env}]
                                       (update env :v
                                               #(cond
                                                  (= 1 (mod y 2)) (calc/inv %)
                                                  (= 0 (mod x 2)) :u
                                                  :else :i)))}
                              {:pos :topcenter :align :topcenter}) [6 3])
           [[:n :n :u :i :u :n]
            [:n :n :m :m :m :n]
            [:n :n :n :n :n :n]]))))

(defn expected-subpattern-size?
  "Note: only for 2D gens where subpattern is aligned `:topleft` and contains no `:n` while the rest of the generation contains only `:n`!"
  [expected-size gen]
  (loop [ptn (first gen)
         rem (rest gen)
         i 0]
    (let [sub-ptn (remove #{:n} ptn)
          expected-width (if (< i (last expected-size))
                           (first expected-size)
                           0)]
      ;; check if pattern width = defined width
      (when (= (count sub-ptn) expected-width)
        (if (seq rem)
          (recur (first rem) (rest rem) (inc i))
          true)))))

(deftest ini-rand-figure-test
  (testing "random variance"
    (is (not= (sys-ini (make-ini :rand-figure :n 10
                                 {:pos :left :align :left}) [14])
              (sys-ini (make-ini :rand-figure :n 10
                                 {:pos :left :align :left}) [14])
              (sys-ini (make-ini :rand-figure :n 10
                                 {:pos :left :align :left}) [14])))
    (is (not= (sys-ini (make-ini :rand-figure :n 3
                                 {:pos :center :align :center}) [5 5])
              (sys-ini (make-ini :rand-figure :n 3
                                 {:pos :center :align :center}) [5 5]))))
  (testing "random reproducability"
    (is (= (sys-ini (make-ini :rand-figure :n 10
                              {:pos :left :align :left}) [14] {:seed 42})
           (sys-ini (make-ini :rand-figure :n 10
                              {:pos :left :align :left}) [14] {:seed 42})
           (sys-ini (make-ini :rand-figure :n 10
                              {:pos :left :align :left}) [14] {:seed 42}))))
  (testing "correct pattern size"
    (is (let [size [3 6]
              ini (sys-ini (make-ini :rand-figure {:weights 1.0}
                                     :n size :topleft)
                           [6 10])]
          (expected-subpattern-size? size ini)))
    (is (let [size [5 2]
              ini (sys-ini (make-ini :rand-figure {:weights 1.0}
                                     :n size :topleft)
                           [5 3])]
          (expected-subpattern-size? size ini)))
    (is (let [size [3 3]
              ini (sys-ini (make-ini :rand-figure {:weights 1.0}
                                     :n (first size) :topleft)
                           [5 4])]
          (expected-subpattern-size? size ini)))))

(deftest ini-comp-figures-test
  (testing "ini composition"
    (is (= (sys-ini (make-ini :comp-figures :n
                              [(make-ini :figure :n [:u :i] 1)
                               (make-ini :figure :n (ini-patterns :ball) 4)
                               (make-ini :figure :n [:i :n :u] :right)])
                    [15])
           [:n :u :i :n :i :u :m :u :i :n :n :n :i :n :u]))))

(deftest ini-figure-repeat-test
  (testing "repetition count and spacing"
    (is (= (sys-ini (make-ini :figure-repeat :n [:m :u] {:pos 0 :align :left}
                              3 2)
                    [15])
           [:m :u :n :n :m :u :n :n :m :u :n :n :n :n :n]))
    (is (= (sys-ini (make-ini :figure-repeat {:weights 1.0}
                              :n (vec (repeat 3 (vec (repeat 3 :?)))) :center
                              3 1)
                    [13 13] {:seed 42})
           [[:n :n :n :n :n :n :n :n :n :n :n :n :n]
            [:n :u :m :i :n :u :m :u :n :u :m :i :n]
            [:n :i :m :i :n :i :u :m :n :i :u :m :n]
            [:n :m :u :m :n :i :m :m :n :m :i :m :n]
            [:n :n :n :n :n :n :n :n :n :n :n :n :n]
            [:n :u :m :i :n :u :m :u :n :m :m :m :n]
            [:n :u :i :i :n :i :m :i :n :u :u :m :n]
            [:n :m :u :m :n :m :m :i :n :i :u :u :n]
            [:n :n :n :n :n :n :n :n :n :n :n :n :n]
            [:n :i :u :m :n :i :m :i :n :i :i :i :n]
            [:n :i :m :m :n :u :i :u :n :m :m :u :n]
            [:n :u :u :u :n :u :u :i :n :m :i :i :n]
            [:n :n :n :n :n :n :n :n :n :n :n :n :n]]))))


;; Umwelt tests

(deftest observe-umwelt-test
  (testing "Correct umwelt observation"
    (is (= (observe-umwelt (make-umwelt :select-ltr 1)
                           [:n :u :n :i :n] [[2] :n])
           [:n]))
    (is (= (observe-umwelt (make-umwelt :select-ltr 2)
                           [:n :u :n :i :n] [[2] :n])
           [:u :i]))
    (is (= (observe-umwelt (make-umwelt :select-ltr 3)
                           [:n :m :u :i :n :m :i] [[3] :u])
           [:u :i :n]))
    (is (= (observe-umwelt (make-umwelt :select-ltr 4)
                           [:n :m :u :i :n :m :i] [[3] :u])
           [:m :u :n :m]))
    (is (= (observe-umwelt (make-umwelt :select-ltr 5)
                           [:n :m :u :i :n :m :i] [[3] :u])
           [:m :u :i :n :m]))
    (is (= (observe-umwelt (make-umwelt :self-select-ltr 3)
                           [[:n :i :n :u :i]
                            [:u :m :n :i :m]
                            [:m :n :i :m :u]
                            [:n :n :u :i :m]
                            [:m :u :i :i :n]] [[2 2] :i]) ; :i looks up
           [:m :n :i]))
    (is (= (observe-umwelt (make-umwelt :self-select-ltr 4)
                           [[:n :i :n :u :i]
                            [:u :m :n :i :m]
                            [:m :n :m :m :u]
                            [:n :n :u :u :m]
                            [:m :u :i :n :n]] [[2 2] :m]) ; :m looks right
           [:u :i :u :n]))
    (is (= (observe-umwelt (make-umwelt :moore :column-first false)
                           [[:n :n :u]
                            [:m :n :n]
                            [:n :n :i]] [[1 1] :n])
           [:n :m :n :n :n :u :n :i]))
    (is (= (observe-umwelt (make-umwelt :moore :row-first true)
                           [[:n :n :u]
                            [:m :n :n]
                            [:n :n :i]] [[1 1] :n])
           [:n :n :u :m :n :n :n :n :i]))
    (is (= (observe-umwelt (make-umwelt :von-neumann :column-first true)
                           [[:n :n :u]
                            [:m :n :u]
                            [:n :i :n]] [[1 1] :n])
           [:m :n :n :i :u]))
    (is (= (observe-umwelt (make-umwelt :von-neumann :row-first false)
                           [[:n :n :u]
                            [:m :n :u]
                            [:n :i :n]] [[1 1] :n])
           [:n :m :u :i]))
    ))

(deftest apply-rule-test
  (testing "Correct :match application"
    (is (= (apply-rule (make-rule :match [:n :n :n :n
                                          :n :n :m :n
                                          :n :n :n :n
                                          :n :n :n :n])
                       [:u :i]
                       [[0 0] :n]) ; cell doesn’t matter here
           :m))
    (let [dna [:n :i :u :m  :i :n :u :n  :n :u :n :n  :u :n :u :m
               :n :u :n :n  :n :u :i :u  :u :n :m :i  :m :m :u :u
               :m :n :n :m  :i :n :i :n  :n :u :m :n  :m :m :i :n
               :i :u :n :i  :i :i :n :m  :m :n :m :i  :m :i :u :n]
          match (make-rule :match dna)]
      (is (= dna (mapv #(apply-rule match (vec %) [[0 0] :n])
                       (calc/vspace 3))))))
  (testing "Correct :life application"
    (let [life (make-rule
                :life
                [:n :u :i :m  :n :n :i :i  :n :u :n :u  :n :n :n :n
                 :n :u :i :m  :n :u :i :m  :n :u :n :u  :n :u :n :u
                 :n :u :i :m  :n :n :i :i  :n :u :i :m  :n :n :i :i
                 :n :u :i :m  :n :u :i :m  :n :u :i :m  :n :u :i :m])]
      (are [umwelt result] (= (apply-rule life umwelt [[0 0] :m]) result)
        [:n :m :n :n nil :n :u :n :i] :i ; alive: [:m :u :i]
        [:n :u :n :m nil :m :n :n :n] :u ; alive: [:u :m :m]
        [:n :m :n :n nil :n :u :n :n] :m ; alive: [:m :u]
        [:n :n :n :n nil :n :u :n :n] :n ; alive: [:u]
        [:n :m :n :n nil :n :u :m :i] :n ; alive: [:m :u :m :i]
        ))))


;; NOTE: do not call `--fast` methods like this unless you know what you do!
;;       They expect different input data and must be called together.
(deftest optimized-method-test
  (let [->umwelt (fn [size gen1d cell]
                   (i/observe-umwelt--fast
                    (make-umwelt :select-ltr size)
                    (utils/keywords-to-array gen1d)
                    cell (count gen1d)))]
    (testing "Correct :match application"
      (is (= (i/apply-rule--fast (make-rule :match [:n :n :n :n
                                                    :n :n :m :n
                                                    :n :n :n :n
                                                    :n :n :n :n])
                                 (->umwelt 2 [:n :u :n :i :n] [[2] :n]) ;=> "12"
                                 :n)
             :m))
      (let [dna [:n :i :u :m  :i :n :u :n  :n :u :n :n  :u :n :u :m
                 :n :u :n :n  :n :u :i :u  :u :n :m :i  :m :m :u :u
                 :m :n :n :m  :i :n :i :n  :n :u :m :n  :m :m :i :n
                 :i :u :n :i  :i :i :n :m  :m :n :m :i  :m :i :u :n]
            match (make-rule :match dna)]
        (is (= dna (mapv (comp #(i/apply-rule--fast match % :n)
                               #(->umwelt 3 (vec %) [[1] :n]))
                         (calc/vspace 3)))))))
  (let [->umwelt (fn [gen2d cell]
                   (i/observe-umwelt--fast
                    (make-umwelt :moore :column-first false)
                    (utils/keywords-to-array-2d gen2d)
                    cell (count (first gen2d)) (count gen2d)))]
    (testing "Correct :life application"
      (let [life (make-rule
                  :life
                  [:n :u :i :m  :n :n :i :i  :n :u :n :u  :n :n :n :n
                   :n :u :i :m  :n :u :i :m  :n :u :n :u  :n :u :n :u
                   :n :u :i :m  :n :n :i :i  :n :u :i :m  :n :n :i :i
                   :n :u :i :m  :n :u :i :m  :n :u :i :m  :n :u :i :m])]
        (are [gen result] (= (i/apply-rule--fast
                              life
                              (->umwelt gen [[1 1] :m]) :m)
                             result)
          [[:n :u :n]
           [:n :m :i]
           [:m :n :n]] :i  ; alive: [:m :u :i]
          [[:n :m :n]
           [:u :m :n]
           [:n :m :n]] :u  ; alive: [:u :m :m]
          [[:n :m :n]
           [:n :m :n]
           [:n :n :u]] :m  ; alive: [:m :u]
          [[:n :n :n]
           [:n :m :n]
           [:u :n :n]] :n  ; alive: [:u]
          [[:m :n :n]
           [:n :m :i]
           [:u :m :n]] :n  ; alive: [:m :u :m :i]
          )))))

(deftest spec-integration-test
  (let [dna [:n :i :u :m  :i :n :u :n  :n :u :n :n  :u :n :u :m
             :n :u :n :n  :n :u :i :u  :u :n :m :i  :m :m :u :u
             :m :n :n :m  :i :n :i :n  :n :u :m :n  :m :m :i :n
             :i :u :n :i  :i :i :n :m  :m :n :m :i  :m :i :u :n]
        rule-spec   (make-rule :match dna)
        umwelt-spec (make-umwelt :select-ltr 3)
        ini-spec    (make-ini :random)
        ->ca-spec   #(specify-ca "test-specimen"
                                 {:rule-spec rule-spec
                                  :umwelt-spec umwelt-spec
                                  :ini-spec ini-spec})]
    (testing "Working integration of ini, rule and umwelt in sys-next"
      (is (= (let [init-gen (sys-ini  ini-spec [16] {:seed 64})
                   next-gen (sys-next rule-spec umwelt-spec init-gen)]
               [init-gen next-gen])
             ;; verified
             [[:u :u :i :u :m :i :i :u :u :i :u :i :i :n :u :m]
              [:i :i :n :n :u :m :u :n :i :n :i :m :n :n :n :m]])))
    (testing "Type of CASpec"
      (is (= (type (->ca-spec))
             formform.emul.core.CASpec)))))

(deftest iterator-vs-automaton-equivalence-test
  (let [ca-spec (common-specimen :Mark1)
        res [19]
        ca-iter (ca-iterator ca-spec res)
        ca-autm (create-ca ca-spec res)]
    (testing "Initial conditions"
      (is (= [19] (get-resolution ca-autm)))
      (is (= 0 (get-system-time ca-autm)))
      (is (= (get-current-generation ca-autm)
             (first ca-iter))))
    (testing "Next generation"
      (step ca-autm)
      (is (= 1 (get-system-time ca-autm)))
      (is (= (get-current-generation ca-autm)
             (second ca-iter)))
      (step ca-autm)
      (is (= 2 (get-system-time ca-autm)))
      (is (= (get-current-generation ca-autm)
             (nth ca-iter 2))))
    (testing "Correct restart"
      (is (= 2 (get-system-time ca-autm)))
      (restart ca-autm)
      (is (= 0 (get-system-time ca-autm)))
      (is (= (get-current-generation ca-autm)
             (first ca-iter))))
    (testing "Cached history"
      (is (= 421052 (get-history-cache-limit ca-autm)))
      (is (= (get-cached-history ca-autm)
             (take 3 ca-iter))))))


;; Notes about the test data:

;; All CA snapshots are loaded as EDN data and compared to the generated evolutions for each test case.

;; SelFi snapshots are generated with initial conditions from various examples in the appendix of uFORM iFORM (trimmed down to small excerpts) and carefully compared visually to the original images (some only partially).

;; Snapshots for mindforms, lifeforms and decisionforms have been extracted (using my `ffemul-img2data` tool) from screenshots of videos from:
;; - https://uformiform.info/en/downloads (mindforms)
;; - https://www.youtube.com/channel/UCZjKMohiTT1Tjo5z2BmB18g/videos (lifeforms, decisionforms)

;; The Clojure library Quil (https://clojars.org/quil) is useful to visualize the snapshot data if you want to verify for yourself.


(def selfi-snaps (edn/read-string
                  (slurp "./test/formform/emul/snapshots/selfi_snapshots.edn")))

(def mindform-snaps
  {:100000mindFORM1-0000s
   (edn/read-string
    (slurp "./test/formform/emul/snapshots/100000mindFORM1_0000s.edn"))
   :100000mindFORM1-0036s
   (edn/read-string
    (slurp "./test/formform/emul/snapshots/100000mindFORM1_0036s.edn"))
   :100101mindFORM_0000s
   (edn/read-string
    (slurp "./test/formform/emul/snapshots/100101mindFORM_0000s.edn"))
   :100101mindFORM_0015s
   (edn/read-string
    (slurp "./test/formform/emul/snapshots/100101mindFORM_0015s.edn"))})

(def lifeform-snaps
  {:100101lifeFORMs_0000s
   (edn/read-string
    (slurp "./test/formform/emul/snapshots/100101lifeFORMs_0000s.edn"))
   :101101circulator_0000s
   (edn/read-string
    (slurp "./test/formform/emul/snapshots/100101lifeFORMs_0000s.edn"))})

(def decisionform-snaps
  {:100101decisionFORMs_0000s
   (edn/read-string
    (slurp "./test/formform/emul/snapshots/100101decisionFORMs_0000s.edn"))})

(defn snapshots->ca
  [species-type snapshots dna]
  (let [snapshot-evol (mapv :data snapshots)
        {:keys [data size]} (snapshots 0)
        ini (make-ini :figure :n data {})
        ca-spec
        (condp = species-type
          :lifeform (make-lifeform dna {:overwrites {:ini-spec ini}})
          :decisionform (make-decisionform dna 10 {:overwrites {:ini-spec ini}})
          :selfi (make-selfi dna ini)
          :mindform (make-mindform dna ini)
          :else (throw (ex-info "Invalid species type."
                                {:species species-type})))]
    [snapshot-evol ca-spec size]))

(defn equiv-iterator-evolution?
  [evol ca res]
  (let [iterator (ca-iterator ca res)]
    (= (take (count evol) iterator) evol)))

(defn equiv-automaton-evolution?
  [evol ca res]
  (let [automaton (create-ca ca (count evol) res)]
    (dotimes [_ (count (rest evol))] (step automaton))
    (= (get-cached-history automaton {:optimized? false})
       evol)))

;; alternative method:
#_
(defn equiv-automaton-evolution?
  ca-data
  (let [automaton (create-ca ca 0)
        init-gen (get-current-generation automaton false)]
    (= (cons init-gen (repeatedly (count (rest evol))
                                  (fn []
                                    (step automaton)
                                    (get-current-generation automaton false))))
       evol)))


(deftest mindform-congruence
  (testing "100000mindFORM1 snapshot congruence with iterator"
    (let [ca-data (snapshots->ca :mindform
                                 (mindform-snaps :100000mindFORM1-0000s)
                                 (tsds-sel->dna [1 0 0 0 0 0]))]
      (is (apply equiv-iterator-evolution? ca-data))
      (is (apply equiv-automaton-evolution? ca-data)))

    ;; tests should pass, but left out since they’re too slow:
    #_
    (let [ca-data (snapshots->ca :mindform
                                 (mindform-snaps :100000mindFORM1-0036s)
                                 (tsds-sel->dna [1 0 0 0 0 0]))]
      (is (apply equiv-iterator-evolution? ca-data))
      (is (apply equiv-automaton-evolution? ca-data))))

  (testing "100101mindFORM snapshot congruence with iterator"
    (let [ca-data (snapshots->ca :mindform
                                 (mindform-snaps :100101mindFORM_0000s)
                                 (tsds-sel->dna [1 0 0 1 0 1]))]
      (is (apply equiv-iterator-evolution? ca-data))
      (is (apply equiv-automaton-evolution? ca-data)))

    ;; tests should pass, but left out since they’re too slow:
    #_
    (let [ca-data (snapshots->ca :mindform
                                 (mindform-snaps :100101mindFORM_0015s)
                                 (tsds-sel->dna [1 0 0 1 0 1]))]
      (is (apply equiv-iterator-evolution? ca-data))
      (is (apply equiv-automaton-evolution? ca-data)))))

(deftest lifeform-congruence
  (testing "100101lifeFORMs_0000s snapshot congruence with iterator"
    (let [ca-data (snapshots->ca :lifeform
                                 (lifeform-snaps :100101lifeFORMs_0000s)
                                 (tsds-sel->dna [1 0 0 1 0 1]))]
      (is (apply equiv-iterator-evolution? ca-data))
      (is (apply equiv-automaton-evolution? ca-data))))

  (testing "101101circulator_0000s snapshot congruence with iterator"
    (let [ca-data (snapshots->ca :lifeform
                                 (lifeform-snaps :101101circulator_0000s)
                                 ;; the TSDS is NOT 101101 but 100101!
                                 (tsds-sel->dna [1 0 0 1 0 1]))]
      (is (apply equiv-iterator-evolution? ca-data))
      (is (apply equiv-automaton-evolution? ca-data)))))

(deftest decisionform-congruence
  (testing "100101decisionFORMs_0000s snapshot congruence with iterator"
    (let [ca-data (snapshots->ca :decisionform
                                 (decisionform-snaps
                                  :100101decisionFORMs_0000s)
                                 (tsds-sel->dna [1 0 0 1 0 1]))]
      (is (apply equiv-iterator-evolution? ca-data))
      (is (apply equiv-automaton-evolution? ca-data)))))

(deftest selfi-congruence
  (testing "Mark1 snapshot congruence"
    (is (= (selfi-snaps :Mark1-r25t20) ;; roughly verified
           (take 20 (ca-iterator (common-specimen :Mark1)
                                 [25])))))
  (testing "Slit snapshot congruence"
    (is (= (selfi-snaps :Slit-r27t20) ;; roughly verified
           (take 20 (ca-iterator (common-specimen :Slit)
                                 [27])))))

  (testing "or4v snapshot congruence"
    (is (= (selfi-snaps :or4v-r21t19) ;; fully verified
           (take 19 (ca-iterator (common-specimen :or4v)
                                 [21])))))

  (testing "Rule4v110 snapshot congruence"
    (is (= (selfi-snaps :xorReId-r27t20) ;; fully verified
           (take 20 (ca-iterator (common-specimen :xorReId)
                                 [27])))))

  (testing "Rule4v30 snapshot congruence"
    (is (= (selfi-snaps :Rule4v30-r27t11) ;; fully verified
           (take 11 (ca-iterator (common-specimen :Rule4v30)
                                 [27])))))

  (testing "Rule4v111 snapshot congruence"
    (is (= (selfi-snaps :Rule4v111-r19t19) ;; fully verified
           (take 19 (ca-iterator (common-specimen :Rule4v111)
                                 [19])))))

  (testing "Structure111Re snapshot congruence"
    (is (= (selfi-snaps :Structure111Re-r27t20) ;; roughly verified
           (take 20 (ca-iterator (common-specimen :Structure111Re)
                                 [27])))))

  (testing "Rule4v110 snapshot congruence"
    (is (= (selfi-snaps :Rule4v110-r25t20) ;; fully verified
           (take 20 (ca-iterator (common-specimen :Rule4v110)
                                 [25]))))))



(comment

  (require '[clojure.pprint :as pprint :refer [pprint]])

  (binding [pprint/*print-right-margin* 80
            pprint/*print-miser-width* 80]
    (pprint (take 20 (ca-iterator (common-specimen :Mark1)
                                  [25]))))

  ,)

(comment

  (let [ca-data (snapshots->ca :lifeform
                               (lifeform-snaps :101101circulator_0000s)
                               (tsds-sel->dna [1 0 0 1 0 1]))]
    (apply equiv-iterator-evolution? ca-data))

  
  (let [ca-data (snapshots->ca :lifeform
                               (lifeform-snaps :100101lifeFORMs_0000s)
                               (tsds-sel->dna [1 0 0 1 0 1]))]
    ;; (apply equiv-iterator-evolution? ca-data)
    (apply equiv-automaton-evolution? ca-data))

  (def snapshot ((lifeform-snaps :100101lifeFORMs_0000s) 0))
  (def ini-gen (:data snapshot))
  (def res (:size snapshot))

  (= (->> (ca-iterator
           (make-lifeform (tsds-sel->dna [1 0 0 1 0 1])
                          {:overwrites {:ini-spec
                                        (make-ini :fill-all ini-gen)}})
           
           [res])
          (take 10))
     (map :data (lifeform-snaps :100101lifeFORMs_0000s)))

  (require '[clojure.math.combinatorics :as combo])
  
  (let [perms (combo/selections [0 1] 6)]
    (some
     (fn [perm]
       (= (->> (ca-iterator
                (make-lifeform (tsds-sel->dna perm)
                               {:overwrites {:ini-spec
                                             (make-ini :fill-all ini-gen)}})
                [res])
               (take 2)
               last)
          (:data ((lifeform-snaps :100101lifeFORMs_0000s) 1))))
     perms))

  
  (let [ca-data (snapshots->ca :mindform
                               (mindform-snaps :100000mindFORM1-0000s)
                               (tsds-sel->dna [1 0 0 0 0 0]))]
    (apply equiv-automaton-evolution? ca-data))

  (def atm (create-ca (make-species :selfi
                                    [:n :n :n :n
                                     :n :i :n :i
                                     :i :i :i :i
                                     :i :u :i :u]
                                    (make-ini :ball))
                      [10]))
  (get-system-time atm)
  (get-resolution atm)
  (step atm)
  (get-current-generation atm {:optimized? false})
  ,)
