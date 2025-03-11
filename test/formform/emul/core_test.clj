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
