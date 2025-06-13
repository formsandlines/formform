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
      (seq-re :<r nil)     (seq-re :<r' nil)     [:u]
      (seq-re :<..r nil)   (seq-re :<..r' nil)   :u
      (seq-re :<..r. nil)  (seq-re :<..r'. nil)  [:u]
      (seq-re :<r_ nil)    (seq-re :<r'_ nil)    [:u]
      (seq-re :<..r_ nil)  (seq-re :<..r'_ nil)  :u
      (seq-re :<..r._ nil) (seq-re :<..r'._ nil) [:u])

    (are [x1 x2 y] (= y (f x1) (f x2))
      (seq-re :<r nil nil)     (seq-re :<r' nil nil)     :u
      (seq-re :<..r nil nil)   (seq-re :<..r' nil nil)   :u
      (seq-re :<..r. nil nil)  (seq-re :<..r'. nil nil)  :u
      (seq-re :<r_ nil nil)    (seq-re :<r'_ nil nil)    [:u]
      (seq-re :<..r_ nil nil)  (seq-re :<..r'_ nil nil)  [:u]
      (seq-re :<..r._ nil nil) (seq-re :<..r'._ nil nil) [:u]))

  (testing "Reduction from all possible ambiguous cases"
    (is (= [:u]
           (f (seq-re :<r :u nil))     (f (seq-re :<r :i nil))
           (f (seq-re :<..r :u nil))   (f (seq-re :<..r :i nil))
           (f (seq-re :<..r. :u nil))  (f (seq-re :<..r. :i nil))
           (f (seq-re :<r_ :u nil))    (f (seq-re :<r_ :i nil))
           (f (seq-re :<..r_ :u nil))  (f (seq-re :<..r_ :i nil))
           (f (seq-re :<..r._ :u nil)) (f (seq-re :<..r._ :i nil))))
    (is (= [:u]
           (f (seq-re :<r'_ :u nil))
           (f (seq-re :<..r'_ :u nil)) (f (seq-re :<..r'._ :u nil))
           (f (seq-re :<r'_ :i nil))
           (f (seq-re :<..r'_ :i nil)) (f (seq-re :<..r'._ :i nil))))
    ;; Exceptions in alternative interpretation
    (is (= :u
           (f (seq-re :<r' :u nil))
           (f (seq-re :<..r' :u nil)) (f (seq-re :<..r'. :u nil))))
    (is (= []
           (f (seq-re :<r' :i nil))
           (f (seq-re :<..r' :i nil)) (f (seq-re :<..r'. :i nil)))))

  (testing "Non-reduction of uninterpreted expressions"
    (are [x y] (= y (f x))
      (seq-re :<r 'a nil)     '[:seq-re :<r a nil]
      (seq-re :<..r 'a nil)   '[:seq-re :<..r a nil]
      (seq-re :<..r. 'a nil)  '[:seq-re :<..r. a nil]
      (seq-re :<r_ 'a nil)    '[:seq-re :<r_ a nil]
      (seq-re :<..r_ 'a nil)  '[:seq-re :<..r_ a nil]
      (seq-re :<..r._ 'a nil) '[:seq-re :<..r._ a nil]

      (seq-re :<r' 'a nil)     '((:u a))
      (seq-re :<..r' 'a nil)   '((:u a))
      (seq-re :<..r'. 'a nil)  '((:u a))
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
      (seq-re :<r'_ 'a)    '(([:u] a))
      (seq-re :<..r'_ 'a)  '((:u a))
      (seq-re :<..r'._ 'a) '(([:u] a)))

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
      (seq-re :<r'_ nil 'a)    '(([:u] a))
      (seq-re :<..r'_ nil 'a)  '(([:u] a))
      (seq-re :<..r'._ nil 'a) '(([:u] a)))

    (are [x y] (= y (f x))
      (seq-re :<r nil 'a nil)     '[:seq-re :<r nil a nil]
      (seq-re :<..r nil 'a nil)   '[:seq-re :<..r nil a nil]
      (seq-re :<..r. nil 'a nil)  '[:seq-re :<..r. nil a nil]
      (seq-re :<r_ nil 'a nil)    '[:seq-re :<r_ nil a nil]
      (seq-re :<..r_ nil 'a nil)  '[:seq-re :<..r_ nil a nil]
      (seq-re :<..r._ nil 'a nil) '[:seq-re :<..r._ nil a nil]

      (seq-re :<r' nil 'a nil)     '(([:u] a))
      (seq-re :<..r' nil 'a nil)   '((:u a))
      (seq-re :<..r'. nil 'a nil)  '(([:u] a))
      (seq-re :<r'_ nil 'a nil)    '[:seq-re :<r'_ nil a nil]
      (seq-re :<..r'_ nil 'a nil)  '[:seq-re :<..r'_ nil a nil]
      (seq-re :<..r'._ nil 'a nil) '[:seq-re :<..r'._ nil a nil]))

  (testing "Reduction to binary FORMs (in case of mark)"
    (is (= '() (f (seq-re :<r 'a nil) {'a :m})))
    (is (= '(a) (f (seq-re :<r [:- [] 'a] 'a))))
    (is (= '(a) (f (seq-re :<r 'b [:- [] 'a] 'a))))
    (is (= '((a) :u) (f (seq-re :<r [:- 'b :i] 'a :u)))))

  (testing "Irreducable cases"
    (is (= '[:seq-re :<r b [:u] a] (f (seq-re :<r 'b [:- :i 'a] 'a))))))


(deftest filter-rems-test
  (testing "Removal of unreferenced shadowed rems"
    (is (= (filter-rems '[[a (x)] [x a] [x :m]] ['x])
           '[[x :m]]))
    (is (= (filter-rems '[[a :n] [x :m] [a (x)]] ['a])
           '[[a (x)] [x :m]]))))


