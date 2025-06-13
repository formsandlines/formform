(ns formform.expr.operators-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [formform.expr.specs :refer [fns-with-specs]]
            [formform.expr.operators :refer :all]
            [orchestra.spec.test :as stest]))

(doseq [fsym fns-with-specs] (stest/instrument fsym))


(defn f
  ([x] (f x {}))
  ([x env] (simplify-seq-reentry x env)))

(deftest simplify-seq-reentry-test
  (testing "Reduction of primitive seq-re types"
    (are [x1 x2 y] (= y (f x1) (f x2))
      (seq-re :<r nil)     (seq-re :<r' nil)     [:U]
      (seq-re :<..r nil)   (seq-re :<..r' nil)   :U
      (seq-re :<..r. nil)  (seq-re :<..r'. nil)  [:U]
      (seq-re :<r_ nil)    (seq-re :<r'_ nil)    [:U]
      (seq-re :<..r_ nil)  (seq-re :<..r'_ nil)  :U
      (seq-re :<..r._ nil) (seq-re :<..r'._ nil) [:U])

    (are [x1 x2 y] (= y (f x1) (f x2))
      (seq-re :<r nil nil)     (seq-re :<r' nil nil)     :U
      (seq-re :<..r nil nil)   (seq-re :<..r' nil nil)   :U
      (seq-re :<..r. nil nil)  (seq-re :<..r'. nil nil)  :U
      (seq-re :<r_ nil nil)    (seq-re :<r'_ nil nil)    [:U]
      (seq-re :<..r_ nil nil)  (seq-re :<..r'_ nil nil)  [:U]
      (seq-re :<..r._ nil nil) (seq-re :<..r'._ nil nil) [:U]))

  (testing "Reduction from all possible ambiguous cases"
    (is (= [:U]
           (f (seq-re :<r :U nil))     (f (seq-re :<r :I nil))
           (f (seq-re :<..r :U nil))   (f (seq-re :<..r :I nil))
           (f (seq-re :<..r. :U nil))  (f (seq-re :<..r. :I nil))
           (f (seq-re :<r_ :U nil))    (f (seq-re :<r_ :I nil))
           (f (seq-re :<..r_ :U nil))  (f (seq-re :<..r_ :I nil))
           (f (seq-re :<..r._ :U nil)) (f (seq-re :<..r._ :I nil))))
    (is (= [:U]
           (f (seq-re :<r'_ :U nil))
           (f (seq-re :<..r'_ :U nil)) (f (seq-re :<..r'._ :U nil))
           (f (seq-re :<r'_ :I nil))
           (f (seq-re :<..r'_ :I nil)) (f (seq-re :<..r'._ :I nil))))
      ;; Exceptions in alternative interpretation
    (is (= :U
           (f (seq-re :<r' :U nil))
           (f (seq-re :<..r' :U nil)) (f (seq-re :<..r'. :U nil))))
    (is (= []
           (f (seq-re :<r' :I nil))
           (f (seq-re :<..r' :I nil)) (f (seq-re :<..r'. :I nil)))))

  (testing "Non-reduction of uninterpreted expressions"
    (are [x y] (= y (f x))
      (seq-re :<r 'a nil)     '[:seq-re :<r a nil]
      (seq-re :<..r 'a nil)   '[:seq-re :<..r a nil]
      (seq-re :<..r. 'a nil)  '[:seq-re :<..r. a nil]
      (seq-re :<r_ 'a nil)    '[:seq-re :<r_ a nil]
      (seq-re :<..r_ 'a nil)  '[:seq-re :<..r_ a nil]
      (seq-re :<..r._ 'a nil) '[:seq-re :<..r._ a nil]

      (seq-re :<r' 'a nil)     '((:U a))
      (seq-re :<..r' 'a nil)   '((:U a))
      (seq-re :<..r'. 'a nil)  '((:U a))
      (seq-re :<r'_ 'a nil)    '[:seq-re :<r'_ a nil]
      (seq-re :<..r'_ 'a nil)  '[:seq-re :<..r'_ a nil]
      (seq-re :<..r'._ 'a nil) '[:seq-re :<..r'._ a nil])

    (are [x y] (= y (f x))
      (seq-re :<r 'a)     '[:seq-re :<r a]
      (seq-re :<..r 'a)   '[:seq-re :<..r a]
      (seq-re :<..r. 'a)  '[:seq-re :<..r. a]
      (seq-re :<r_ 'a)    '[:seq-re :<r_ a]
      (seq-re :<..r_ 'a)  '[:seq-re :<..r_ a]
      (seq-re :<..r._ 'a) '[:seq-re :<..r._ a]

      (seq-re :<r' 'a)     '[:seq-re :<r' a]
      (seq-re :<..r' 'a)   '[:seq-re :<..r' a]
      (seq-re :<..r'. 'a)  '[:seq-re :<..r'. a]
      (seq-re :<r'_ 'a)    '(([:U] a))
      (seq-re :<..r'_ 'a)  '((:U a))
      (seq-re :<..r'._ 'a) '(([:U] a)))

    (are [x y] (= y (f x))
      (seq-re :<r nil 'a)     '[:seq-re :<r nil a]
      (seq-re :<..r nil 'a)   '[:seq-re :<..r nil a]
      (seq-re :<..r. nil 'a)  '[:seq-re :<..r. nil a]
      (seq-re :<r_ nil 'a)    '[:seq-re :<r_ nil a]
      (seq-re :<..r_ nil 'a)  '[:seq-re :<..r_ nil a]
      (seq-re :<..r._ nil 'a) '[:seq-re :<..r._ nil a]

      (seq-re :<r' nil 'a)     '[:seq-re :<r' nil a]
      (seq-re :<..r' nil 'a)   '[:seq-re :<..r' nil a]
      (seq-re :<..r'. nil 'a)  '[:seq-re :<..r'. nil a]
      (seq-re :<r'_ nil 'a)    '(([:U] a))
      (seq-re :<..r'_ nil 'a)  '(([:U] a))
      (seq-re :<..r'._ nil 'a) '(([:U] a)))

    (are [x y] (= y (f x))
      (seq-re :<r nil 'a nil)     '[:seq-re :<r nil a nil]
      (seq-re :<..r nil 'a nil)   '[:seq-re :<..r nil a nil]
      (seq-re :<..r. nil 'a nil)  '[:seq-re :<..r. nil a nil]
      (seq-re :<r_ nil 'a nil)    '[:seq-re :<r_ nil a nil]
      (seq-re :<..r_ nil 'a nil)  '[:seq-re :<..r_ nil a nil]
      (seq-re :<..r._ nil 'a nil) '[:seq-re :<..r._ nil a nil]

      (seq-re :<r' nil 'a nil)     '(([:U] a))
      (seq-re :<..r' nil 'a nil)   '((:U a))
      (seq-re :<..r'. nil 'a nil)  '(([:U] a))
      (seq-re :<r'_ nil 'a nil)    '[:seq-re :<r'_ nil a nil]
      (seq-re :<..r'_ nil 'a nil)  '[:seq-re :<..r'_ nil a nil]
      (seq-re :<..r'._ nil 'a nil) '[:seq-re :<..r'._ nil a nil]))

  (testing "Reduction to binary FORMs (in case of mark)"
    (is (= '() (f (seq-re :<r 'a nil) {'a :M})))
    (is (= '(a) (f (seq-re :<r [:- [] 'a] 'a))))
    (is (= '(a) (f (seq-re :<r 'b [:- [] 'a] 'a))))
    (is (= '((a) :U) (f (seq-re :<r [:- 'b :I] 'a :U)))))

  (testing "Irreducable cases"
    (is (= '[:seq-re :<r b [:U] a] (f (seq-re :<r 'b [:- :I 'a] 'a))))))


(deftest filter-rems-test
  (testing "Removal of unreferenced shadowed rems"
    (is (= (filter-rems '[[a (x)] [x a] [x :M]] ['x])
           '[[x :M]]))
    (is (= (filter-rems '[[a :N] [x :M] [a (x)]] ['a])
           '[[a (x)] [x :M]]))))


