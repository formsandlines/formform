(ns formform.expr.operators-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [formform.expr.specs :refer [fns-with-specs]]
            [formform.expr.symexpr :as symx]
            [formform.expr.operators :refer :all]
            [orchestra.spec.test :as stest]))

(doseq [fsym fns-with-specs] (stest/instrument fsym))

(def simplify-op symx/simplify-op)

(deftest simplify-op-test
  (testing "formDNA"
    (testing "Basic functionality"
      (is (= (simplify-op [:fdna [] [:n]] {}) :n))
      (is (= (simplify-op [:fdna ['a] [:n :u :i :m]] {'a :u}) :u))
      (is (= (simplify-op [:fdna ['a 'b] [:n :u :i :m
                                          :u :i :m :i
                                          :i :m :i :u
                                          :m :i :u :n]] {'a :m})
             '[:fdna [b] [:m :i :u :n]])))

    (testing "Partial matches in dictionary"
      (is (= (simplify-op [:fdna ['a] [:n :u :i :m]] {'x :m})
             '[:fdna [a] [:n :u :i :m]]))
      (is (= (simplify-op [:fdna ['a] [:n :u :i :m]] {'x :m 'a :u})
             :u)))

    (testing "Correctness of transformations"
      (are [x y] (= (simplify-op [:fdna ['a] [:m :i :u :n]] {'a x}) y)
        :n :m
        :u :i
        :i :u
        :m :n)

      (is (= (simplify-op
              [:fdna ['a 'b] [:n :u :i :m
                              :u :i :m :i
                              :i :m :i :u
                              :m :i :u :n]] {'a :u})
             '[:fdna [b] [:u :i :m :i]]))))

  (testing "unclear FORMs"
    (testing "Basic functionality"
      (is (= (simplify-op [:uncl "hey"] {})
             '[:fdna ["hey"] [:u :u :u :n]]))

      (are [x y] (= (simplify-op [:uncl "unkFo"] {"unkFo" x}) y)
        :n :u
        :u :u
        :i :u
        :m :n)

      ;; !! this is correct, but what happened exactly?
      (is (= (simplify-op [:uncl "hey"] {"hey" :m})
             :n))))

  (comment
    (simplify-op [:mem [['a []]] 'a] {}))

  (testing "memory FORMs"
    (testing "Correctness of reduction"
      (is (= (simplify-op [:mem [['a :m]] 'a] {})
             (simplify-op [:mem [['a []]] 'a] {})
             (simplify-op [:mem [['a [[[]]]]] 'a] {})
             '()))
      (is (= (simplify-op [:mem [['a :u]] ['b] 'a] {})
             '[:- (b) :u]))
      (is (= (simplify-op [:mem [['x :n]] 'y] {})
             'y)))

    (testing "Substitution of values from recursive rems"
      ;; (= ctx' ctx) because reduce-by-calling:
      (is (= (simplify-op [:mem [[:x [:- :x]]] :x] {})
             :x))
      ;; ? merge context during repeated substitution or only once afterwards?
      ;; !! too deeply nested
      #_(is (= (simplify-op (make :mem [[:x (make 'a [:- :x])]] :x) {})
               '[:mem [[:x [a :x]]] a :x])))

    (testing "Exception in infinite reduction (stack overflow)"
      ;; infinite recursion because outer expr env nullifies previous dissoc:
      (is (thrown-with-msg? Exception #"Context too deeply nested"
                            (simplify-op [:mem [[:x [:x]]] :x] {})))
      (is (thrown-with-msg? Exception #"Context too deeply nested"
                            (simplify-op [:mem [[:x [:- [:x]]]] :x] {})))
      (is (thrown-with-msg? Exception #"Context too deeply nested"
                            (simplify-op [:mem [[:x [[:- :x]]]] :x] {}))))

    (testing "Combined with outer env"
      (= (simplify-op [:mem [['a :n]] 'a 'b] {'b :u})
         :u))

    (testing "Reduction of shadowed rems"
      (is (= (simplify-op [:mem [['a '(x)] ['b [:- 'a :u]]] [:- 'b]] {})
             '[:- (x) :u])))))

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


