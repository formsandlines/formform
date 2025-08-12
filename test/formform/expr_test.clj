(ns formform.expr-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [formform.calc :as calc]
            [formform.expr.core :as core]
            [formform.expr :refer :all]
            [formform.utils :refer [submap?]]
            [orchestra.spec.test :as stest]))

(doseq [fsym fns-with-specs] (stest/instrument fsym))


(deftest basic-constructor-test
  (testing "Simple input"
    (is (= '[:- x a [:- x :u] b (:m)]
           (make 'x [:- 'a [:- 'x :u] 'b] [:m])))
    (is (= '[x a [:- x :u] b (:m)]
           (form 'x [:- 'a [:- 'x :u] 'b] [:m]))))
  (testing "Operator construction consistency"
    (is (= [:- 'a 'b]
           (make 'a 'b)
           (make :- 'a 'b)
           (make [:- 'a 'b])
           (make (make :- 'a 'b))))
    (is (= [[:- 'a 'b]] ; ? should arrangement be merged
           (form :- 'a 'b)))
    (is (= ['a 'b]
           (form [:- 'a 'b])
           (form (make :- 'a 'b))))
    (is (= [:uncl "apple pie"]
           (make :uncl "apple pie")
           (make [:uncl "apple pie"])
           (make (make :uncl "apple pie"))))
    (is (= [[:uncl "apple pie"]]
           (form :uncl "apple pie")
           (form [:uncl "apple pie"])
           (form (make :uncl "apple pie"))))
    (is (= [:seq-re :<r 'a 'b]
           (make :seq-re :<r 'a 'b)
           (make [:seq-re :<r 'a 'b])
           (make (seq-re :<r 'a 'b))
           (make (make :seq-re :<r 'a 'b))))
    (is (= [[:seq-re :<r 'a 'b]]
           (form :seq-re :<r 'a 'b)
           (form [:seq-re :<r 'a 'b])
           (form (seq-re :<r 'a 'b))
           (form (make :seq-re :<r 'a 'b))))
    (is (= [:fdna [] [:n]]
           (make :fdna [] [:n])
           (make [:fdna [] [:n]])
           (make (make :fdna [] [:n]))))
    (is (= [[:fdna [] [:n]]]
           (form :fdna [] [:n])
           (form [:fdna [] [:n]])
           (form (make :fdna [] [:n]))))))

(deftest special-constructor-test
  (testing "Return value of `make/form-marked`"
    (is (= '[:- [a] [b] [c]]
           (make-marked 'a 'b 'c)))
    (is (= '[[a] [b] [c]]
           (form-marked 'a 'b 'c)))
    (is (= '[:- [x] [a [:- x :u] b] [(:m)]]
           (make-marked 'x [:- 'a [:- 'x :u] 'b] [:m])))
    (is (= '[[x] [a [:- x :u] b] [(:m)]]
           (form-marked 'x [:- 'a [:- 'x :u] 'b] [:m]))))
  (testing "Return value of `make/form-nested-l`"
    (is (= '[:- [[a] b] c]
           (make-nested-l 'a 'b 'c)))
    (is (= '[[[a] b] c]
           (form-nested-l 'a 'b 'c)))
    (is (= '[:- [[x] a [:- x :u] b] (:m)]
           (make-nested-l 'x [:- 'a [:- 'x :u] 'b] [:m])))
    (is (= '[[[x] a [:- x :u] b] (:m)]
           (form-nested-l 'x [:- 'a [:- 'x :u] 'b] [:m]))))
  (testing "Return value of `make/form-nested-r`"
    (is (= '[:- a [b [c]]]
           (make-nested-r 'a 'b 'c)))
    (is (= '[a [b [c]]]
           (form-nested-r 'a 'b 'c)))
    (is (= '[:- x [a [:- x :u] b [(:m)]]]
           (make-nested-r 'x [:- 'a [:- 'x :u] 'b] [:m])))
    (is (= '[x [a [:- x :u] b [(:m)]]]
           (form-nested-r 'x [:- 'a [:- 'x :u] 'b] [:m])))))

(form-nested-r 'a 'b 'c 'd 'e)

(deftest make-op-test
  (testing "Missing operator keyword"
    (is (thrown? java.lang.AssertionError
                 (make-op 'a 'b))))

  (testing "Unknown operator keyword"
    (is (thrown? clojure.lang.ExceptionInfo
                 (make-op :x 'a))))

  (testing "Known operators"
    (is (= '[:- a b] (make-op :- 'a 'b)))
    (is (= '[:* a b] (make-op :* 'a 'b)))
    (is (= '[:| a b] (make-op :| 'a 'b)))
    (is (= '[:uncl "hello"] (make-op :uncl "hello")))
    (is (= '[:uncl "hey [a] you"] (make-op :uncl (make) "hey" ['a] "you")))
    (is (= '[:mem [[x :u]] x] (make-op :mem [['x :u]] 'x)))
    (is (= '[:seq-re :<r a b] (make-op :seq-re :<r 'a 'b))))

  ;; ! needs validation and tests
  (testing "Invalid arguments to known operators"
    (is (= true true))))

(deftest valid-op?-test
  (testing "Context of the test assertions"
    (is (true? (valid-op? (make :fdna ['a 'b]
                                [:m :i :u :n
                                 :i :m :n :u
                                 :u :n :m :i
                                 :n :u :i :m]))))
    (is (true? (valid-op? (make :uncl "Hey"))))
    (is (true? (valid-op? (make :seq-re :<r 'a))))
    (is (true? (valid-op? (make :mem [[:a nil] ['b []]] 'a))))))

(deftest op-data-test
  (testing "Missing operator keyword"
    (is (thrown? java.lang.AssertionError
                 (op-data ['a 'b]))))

  (testing "Unknown operator keyword"
    (is (thrown-with-msg? Exception #"Unknown operator"
                          (op-data [:x 'a]))))

  (testing "Correctness of output"
    (is (= '{:exprs [a b]}
           (op-data (make-op :- 'a 'b))))
    (is (= '{:label "hello"}
           (op-data (make-op :uncl "hello"))))
    (is (= '{:rems [[x :u]], :ctx [x]}
           (op-data (make-op :mem [['x :u]] 'x))))
    (is (= '{:sign :<r, :nested-exprs [a b]}
           (op-data (make-op :seq-re :<r 'a 'b))))))

(deftest op-get-test
  (testing "Missing operator keyword"
    (is (thrown? java.lang.AssertionError
                 (op-get ['a 'b] :foo))))

  (testing "Unknown operator keyword"
    (is (thrown-with-msg? Exception #"Unknown operator"
                          (op-get [:x 'a] :foo))))

  (testing "Correctness of output"
    (is (= '[a b]
           (op-get (make-op :- 'a 'b) :exprs)))
    (is (= "hello"
           (op-get (make-op :uncl "hello") :label)))
    (let [op (make-op :mem [['x :u]] 'x)]
      (is (= '[[x :u]] (op-get op :rems)))
      (is (= '[x] (op-get op :ctx))))
    (let [op (make-op :seq-re :<r 'a 'b)]
      (is (= :<r (op-get op :sign)))
      (is (= '[a b] (op-get op :nested-exprs))))))

(deftest defoperator-test
  (testing "Defining a new operator"
    (do (defoperator :and [a b] [[a] [b]])
        (let [op (make-op :and 'x 'y)]
          (is (= '[:and x y] op))
          (is (= '[[x] [y]] (interpret-op op)))))))


(deftest find-vars-test
  (testing "At root level"
    (is (= '(a)
           (find-vars 'a {})
           (find-vars (make 'a) {})
           (find-vars (make 'a 'a) {})))
    (is (= '(a b)
           (find-vars (make 'a 'b) {})))
    (is (= '("a")
           (find-vars (make "a" "a") {})))
    (is (= '("a" a)
           (find-vars (make "a" 'a) {})))) ;; should this be equal?


  (testing "Empty"
    (is (= '()
           (find-vars :u {})
           (find-vars (make) {})
           (find-vars (form) {'a :m})
           (find-vars (make '(() ()) '()) {}))))

  (testing "Nested"
    (is (= '(a)
           (find-vars (make '(a)) {})
           (find-vars (form 'a) {})))
    (is (= '(a b c)
           (find-vars (make '((a) b) 'c) {}))))

  (testing "In different sequentials"
    (is (= '(x y)
           (find-vars '(x y) {})
           (find-vars '((x (y x))) {})))
    (is (= '(x "y")
           (find-vars (make :fdna ['x "y"] [:n :u :i :m
                                            :n :u :i :m
                                            :n :u :i :m
                                            :n :u :i :m]) {})))
    (is (= '(a x y "b" "z")
           (find-vars (make :mem [['a '(x y)] ["b" "z"]] 'a) {})))
    (is (= '("foo")
           (find-vars (make :uncl "foo") {})))
    (is (= '(a b c)
           (find-vars (seq-re :<r [:- 'a 'b] 'c) {}))))

  (testing "In nested special FORMs"
    (is (= '(a b c)
           (find-vars (seq-re :<r [:- 'a :m] [:- '(b) (seq-re :<..r 'c)]) {})))
    (is (= '(a b c d)
           (find-vars (make :mem [['a (make :mem [['b :u]] 'c)]] 'd) {}))))

  (testing "Differentiated from other elements"
    (is (= '(a)
           (find-vars (make :n 'a nil) {}))))

  (testing "Variable ordering"
    (is (= '(a "b" c "d")
           (find-vars (make 'a "b" 'c "d") {:ordered? false})
           (find-vars (make 'a "b" 'c "d") {:ordered? true})))
    (is (= '("b" c a "d")
           (find-vars (make "b" 'c 'a "d") {:ordered? false})
           (find-vars (make "b" 'c 'a "d") {})))
    (is (= '(a "b" c "d")
           (find-vars (make "b" 'c 'a "d") {:ordered? true}))))

  (testing "Specific variables"
    (is (= '(x)
           (find-vars (make 'a [:x ['x]] "z" ["x" 'y]) {:vars #{'x "y" 'z}}))))

  (testing "Exotic variable names"))
;; incorrect ordering due to symbols
;; (is (= (find-vars '[ "apple tree" ("something" else) ])
;;        '("apple tree" else "something")))


(deftest simplify-test
  (testing "Simple content"
    (testing "FORM"
      (testing "equal"
        (are [x y] (= x y)
          (>> nil) nil
          (>> '()) '()))
      (testing "marked"
        (are [x y] (= x y)
          (>> '(nil))  '()
          (>> '(()))   nil
          (>> '(([]))) [])))

    (testing "Constant"
      (testing "equal"
        (are [x y] (= x y)
          (>> :n) nil
          (>> :m) '()
          (>> :u) :u
          (>> :i) '(:u)))
      (testing "marked"
        (are [x y] (= x y)
          (>> '(:n)) '()
          (>> '(:m)) nil
          (>> '(:u)) '(:u)
          (>> '(:i)) :u)))

    (testing "variable"
      (testing "without env"
        (is (= (>> 'a) 'a))
        (is (= (>> "a") "a")))

      (testing "with env"
        (is (= (>> 'a {'a :m})
               (>> '(a) {'a :n})
               (>> '((a (b))) {'a :i 'b :i})
               '()))
        (is (= (>> "a" {"a" :u}) :u)))))

  (testing "Nested content"
    (testing "reduced by calling"
      (is (= (>> '(nil () nil () nil))
             (>> '(() :m (:n)))
             nil))
      (is (= (>> '(:u nil :u :u))
             '(:u)))
      (is (= (>> '(:i nil :i (:u)))
             :u))
      (is (= (>> '("foo" "foo" "bar" "foo"))
             '("foo" "bar")))
      (is (= (>> '(a b b a b a))
             '(a b)))
      (is (= (>> '((a) (a) (a)))
             'a))
      (is (= (>> '((a (b)) (a (b))))
             '((a (b))))))

    (testing "reduced by crossing"
      (is (= (>> '(()))
             (>> '((nil)))
             (>> '(((:m))))
             (>> '((((nil) nil) nil) nil))
             nil))
      (is (= (>> '((())))
             (>> '(((nil))))
             (>> '(((((:n))))))
             (>> '(((((nil) nil) nil) nil) nil))
             '()))
      (is (= (>> '(((()) (()))))
             (>> '((((nil)) (:m))))
             (>> '(((:u)) ((:i))))
             nil))
      (is (= (>> '(((a))))
             '(a)))
      (is (= (>> '((a)))
             'a))
      (is (= (>> '((x)) {'x :u})
             :u))
      (is (= (>> '(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((x))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) {'x :u})
             '(:u))))

    (testing "irreducable"
      (is (= (>> '(a b))
             '(a b)))
      ;; should these be reducable? pattern-matching?
      ;; -> might get too complex for eval
      (is (= (>> '((a (b)) ((b) a)))
             '((a (b)) ((b) a)))))))


(deftest simplify-in-test
  (testing "Relations of simple expressions"
    (testing "FORMs"
      (is (= (in>> [ nil nil ]) []))
      (is (= (in>> [ '() '() ]) [ '()])))

    (testing "Constants"
      (is (= (in>> [ :n :n ]) []))
      (is (= (in>> [ :m :m ]) [ '()]))
      (is (= (in>> [ :u :u ]) [ :u]))
      (is (= (in>> [ [:u] [:u] ]) [ '(:u)]))
      (is (= (in>> [ :i :i ]) [ '(:u)]))
      (is (= (in>> [ [:i] [:i] ]) [ :u]))
      (is (= (in>> [ :m :n ]) [ '()]))
      (is (= (in>> [ :u :n ]) [ :u]))
      (is (= (in>> [ :i :n ]) [ '(:u)]))
      (is (= (in>> [ :u :i ]) [ '()]))
      (is (= (in>> [ [:u] :i ]) [ '(:u)]))
      (is (= (in>> [ [:i] :u ]) [ :u]))
      (is (= (in>> [ [:u] [:i] ]) [ '()]))
      (is (= (in>> [ :u :m ]) [ '()]))
      (is (= (in>> [ :i :m ]) [ '()])))

    (testing "Variables"
      (testing "without env"
        (is (= (in>> [ 'a 'b])
               [ 'a 'b]))
        (is (= (in>> [ 'a 'a])
               [ 'a]))
        (is (= (in>> [ 'a "a"]) ;; should be equal?
               [ 'a "a"])))

      (testing "with env"
        (is (= (in>> [ 'a 'a ] {'a :u})
               [ :u]))
        (is (= (in>> [ 'a 'b ] {'a :u 'b :i})
               [ '()]))
        (is (= (in>> [ 'a "a" ] {'a :u "a" :i}) ;; should be equal?
               [ '()]))
        (is (= (in>> [ 'a 'b ] {'a :u})
               [ :u 'b]))
        (is (= (in>> [ 'x ] {'x 'y})
               [ 'y])))

      (testing "with recursive env"
        ;; ? infinite recursion or dissoc from env on first interpretation
        (is (= (in>> [ 'x ] {'x 'x})
               (in>> [ 'x 'x ] {'x 'x})
               '[x])
            (= (in>> [ 'x ] {'x [:- 'x]})
               '[x]))))

    (testing "mixed"
      (is (= (in>> [ '(()) nil :n '(((()))) '(() () ())])
             []))
      (is (= (in>> [ '((() ())) :m '()])
             [ '()]))
      (is (= (in>> [ :u '((:u)) :u '((:u) (:u))])
             [ :u]))
      (is (= (in>> [ '(:u) '(((:u))) :i '((:i) (:i))])
             [ '(:u)]))
      (is (= (in>> [ :u '(:i)])
             [ :u]))
      (is (= (in>> [ '(:i) '(:u)])
             [ '()]))
      (is (= (in>> [ '((:u)) '(:u)])
             [ '()]))
      (is (= (in>> [ :u '(:i)])
             [ :u]))
      (is (= (in>> [ '(:u) '((:u))])
             [ '()]))
      (is (= (in>> [ '((:i)) :u])
             [ '()]))))

  (testing "Relations of complex expressions"
    (testing "pure FORMs"
      (is (= (in>> [ '(())])
             []))
      (is (= (in>> [ '(() ())])
             []))
      (is (= (in>> [ '() '(() ())])
             [ '()]))
      (is (= (in>> [ '(((() (() ()))) ((() (())) (() ()) (())))])
             [])))

    (testing "nested variables (reflexion rule)"
      (is (= (in>> '[ ((a))])
             [ 'a]))
      (is (= (in>> '[ ((a b c))])
             [ 'a 'b 'c]))
      (is (= (in>> '[ ((a ((b c)) d)) ((e ((f))))])
             [ 'a 'b 'c 'd 'e 'f]))
      (is (= (in>> '[ (a ((b c)) d) ((e (f)))])
             [ '(a b c d) 'e '(f)]))
      (is (= (in>> [ '([:- a]) '[:- a]])
             [ '()]))
      (is (= (in>> [ '([:- a]) '[:- b]])
             [ '(a) 'b])))

    (testing "nested U/I relations (generation rule)"
      (is (= (in>> '[ :u (:i)])
             [ :u]))
      (is (= (in>> '[ :u (:n (:i))])
             [ '()]))
      (is (= (in>> '[ :u (:n (:n (:i)))])
             [ :u]))
      (is (= (in>> '[ :u (:n (:u (:n (:i))))])
             [ '()]))
      (is (= (in>> '[ :n (:u (:n (:n (:i))))])
             [ '(:u)]))
      (is (= (in>> '[ :i (:u)])
             [ '(:u)]))
      (is (= (in>> '[ :i (:n (:u))])
             [ '()]))
      (is (= (in>> '[ :i (:n (:n (:u)))])
             [ '(:u)]))
      (is (= (in>> '[ :i (:n (:i (:n (:u))))])
             [ '()]))
      (is (= (in>> '[ :n (:i (:n (:n (:u))))])
             [ :u]))
      (is (= (in>> '[ :u (:i a)])
             [ :u]))
      (is (= (in>> '[ :u ((a :u) :i)])
             [ :u]))
      (is (= (in>> '[ :u ((a :i) :u)])
             [ '()]))
      (is (= (in>> '[ :i ((a (b :u (c))) c)])
             [ '(:u) '((a) c)]))))

  (testing "Degeneration in nested expressions"
    (is (= (in>> '[ a b (a c (c a d)) ] {})
           '[ a b (c (d))]))
    (is (= (in>> '[ (a (b a c) (a c (b c d a))) ] {})
           '[ (a (b c) (c (b d)))]))
    (is (= (in>> '[ x (a (b x)) (y (a x (b x)))])
           '[ x (a (b)) (y)])))

  (testing "Long relations" ;; ? remove due to triviality
    (is (= (in>> (repeat 100 nil)) []))
    (is (= (in>> (repeat 100 '())) [ '()]))
    (is (= (in>> (repeat 100 :u)) [ :u]))
    (is (= (in>> (repeat 100 '(:u))) [ '(:u)]))
    (is (= (in>> (repeat 100 :n)) []))
    (is (= (in>> (repeat 100 :m)) [ '()]))
    (is (= (in>> (repeat 100 :u)) [ :u]))
    (is (= (in>> (repeat 100 :i)) [ '(:u)])))

  (testing "Operators in context"
    (is (= '[[:seq-re :<r a b]]
           (in>> [[:- [:seq-re :<r 'a 'b]]])))
    ;; law of crossing cannot be applied to (known) operators
    (is (= '[(a b c)]
           (in>> [ [[:- 'a 'b 'c]]])))
    (is (= '[:? y] ;; operator unknown
           (in>> [ [[:? 'y]]])))
    (is (= '[([:fdna ["foo"] [:u :u :u :n]])]
           (in>> [ [[:uncl "foo"]]])))
    (is (= '[([:seq-re :<..r a b])]
           (in>> [ [[:seq-re :<..r 'a 'b]]])))
    (is (= '[(z)]
           (in>> [ [[:mem [['x 'y]] 'z]]])))
    ;; law of calling can be applied to (all?) operators
    ;; (although there are practical limitations in simplification algo)
    (is (= '[a b c]
           (in>> [[:- 'a 'b 'c] [:- 'a 'b 'c]])))
    (is (= '[z]
           (in>> [[:mem [['x 'y]] 'z] [:mem [['x 'y]] 'z]])
           (in>> [[:mem [['x 'y] ['y 'z]] 'x] [:mem [['x 'y] ['y 'z]] 'x]])))
    (is (= '[:u]
           (in>> [[:mem [['x :u]] 'x] [:mem [['x :u]] 'x]])))
    (is (= '[[:fdna ["foo"] (:u :u :u :n)]]
           (in>> [[:uncl "foo"] [:uncl "foo"] [:uncl "foo"]])
           (in>> [[:fdna ["foo"] [:u :u :u :n]
                   [:fdna ["foo"] [:u :u :u :n]]]])))
    (is (= '[[:seq-re :<..r a b]]
           (in>> [[:seq-re :<..r 'a 'b] [:seq-re :<..r 'a 'b]])))
    ;; cannot cross re-entry boundary in (de)generation!
    (is (= '[[:seq-re :<r nil a] a]
           (in>> [[:seq-re :<r 'a 'a] 'a])
           (in>> [[:- [:seq-re :<r 'a 'a] 'a]])))
    (is (= '[[:seq-re :<r [:u] a] :u]
           (in>> [[:seq-re :<r :i 'a] :u])
           (in>> [[:seq-re :<r [:- :i] 'a] :u])))
    (is (= '[[:seq-re :<r a :u] [:u]]
           (in>> [[:seq-re :<r 'a :u] :i])
           (in>> [[:seq-re :<r 'a [:- :u]] :i])))
    ;; dominance of mark still holds
    (is (= '[[]]
           (in>> [[:seq-re :<r 'a 'b] []])
           (in>> [[] [:seq-re :<r 'a 'b]])
           (in>> [[] [[[:seq-re :<r 'a 'b]]]])
           (in>> [:u [:seq-re :<r 'a 'b] :i])))
    (is (= '[[]]
           (in>> [[:seq-re :<r nil nil] :i])
           (in>> [[:seq-re :<r nil] :u])
           (in>> [[:seq-re :<r nil nil] [:seq-re :<r nil]])))
    (is (= '[[]]
           (in>> [[:- [:seq-re :<r 'a 'b] []]])
           (in>> [[:fdna [] [:m]] [:seq-re :<r 'a 'b]])
           (in>> [[:fdna ["foo"] [:n :u :u :u]] [:fdna [] [:m]]])))))

;; more exhaustive tests in formform.expr.core-test
;; the tests below are more useful to illustrate how the functions work:
(deftest simplify-nested-test
  (testing "Simple cases in rightwards-nesting"
    (is (= '[nil]   (nested-r>> '( ))))       ; () => ()
    (is (= '[nil]   (nested-r>> '( nil ))))   ; () => ()
    (is (= '[[]]    (nested-r>> '( [] ))))    ; (()) => (())
    (is (= '[a b c] (nested-r>> '( a b c )))) ; (a (b (c))) => (a (b (c)))

    (is (= '[[]]  (nested-r>> '( () a b )))) ; (() (a (b))) => (())
    (is (= '[a]   (nested-r>> '( a () b )))) ; (a (() (b))) => (a)
    (is (= '[a b] (nested-r>> '( a b () )))) ; (a (b (()))) => (a (b))

    (is (= '[nil a b]  (nested-r>> '( nil a b )))) ; ((a (b))) => ((a (b)))
    ;; keep in mind that the output is still a nested chain,
    ;; so `a b` must be wrapped in an arrangement: `[:- a b]`
    (is (= '[[:- a b]] (nested-r>> '( a nil b )))) ; (a ((b))) => (a b)
    (is (= '[a]        (nested-r>> '( a b nil )))) ; (a (b ())) => (a)
    )

  (testing "Simple cases in leftwards-nesting"
    (is (= '[nil]   (nested-l>> '( ))))       ; () => ()
    (is (= '[nil]   (nested-l>> '( nil ))))   ; () => ()
    (is (= '[[]]    (nested-l>> '( [] ))))    ; (()) => (())
    (is (= '[a b c] (nested-l>> '( a b c )))) ; (((a) b) c) => (((a) b) c)

    (is (= '[a b] (nested-l>> '( () a b )))) ; (((()) a) b) => ((a) b)
    (is (= '[b]   (nested-l>> '( a () b )))) ; (((a) ()) b) => (b)
    (is (= '[[]]  (nested-l>> '( a b () )))) ; (((a) b) ()) => (())

    (is (= '[b]        (nested-l>> '( nil a b )))) ; ((() a) b) => (b)
    ;; keep in mind that the output is still a nested chain,
    (is (= '[[:- a b]] (nested-l>> '( a nil b )))) ; (((a)) b) => (a b)
    (is (= '[a b nil]  (nested-l>> '( a b nil )))) ; (((a) b)) => (((a) b))
    )

  (testing "Correct transformation to nested FORM"
    (is (= '[a [b [c]]] (apply form-nested-r (nested-r>> '( a b c )))))
    (is (= '[:- a [b [c]]] (apply make-nested-r (nested-r>> '( a b c )))))
    (is (= '[[[a] b] c] (apply form-nested-l (nested-l>> '( a b c )))))
    (is (= '[:- [[a] b] c] (apply make-nested-l (nested-l>> '( a b c )))))

    (is (= '[a [b]] (apply form-nested-r (nested-r>> '( a b () )))))
    (is (= '[[]] (apply form-nested-r (nested-r>> '( a nil () )))))
    (is (= '[[a] b] (apply form-nested-l (nested-l>> '( () a b )))))
    (is (= '[[]] (apply form-nested-l (nested-l>> '( () nil a )))))

    (is (= '[a b] (apply form-nested-l (nested-l>> '( a nil b )))))
    (is (= '[:- a b] (apply make-nested-l (nested-l>> '( a nil b )))))))


(deftest eval->x-test
  (testing "Reducable expressions"
    (testing "FORMs"
      (are [x y] (= x (=> y) (==> y))
        :n (make nil)
        :m (make '())
        :u (make :u)
        :i (make '(:u))))

    (testing "Constants"
      (are [x y] (= x (=> y) (==> y))
        :n (make :n)
        :m (make :m)
        :u (make :u)
        :i (make :i))))

  (testing "irreducable expressions"
    (is (= 'a (=> (make 'a))))
    (is (= :x (=> [[:x]])))
    (is (= '("x" ("y")) (=> (make '("x" ("y"))))))
    (is (= :_ (==> (make 'a))))
    (is (= :_ (==> [[:x]])))
    (is (= :_ (==> (make '("x" ("y"))))))))

(defn ->nmui [fdna-expr]
  (let [{:keys [varorder dna]} (op-data fdna-expr)]
    (apply str varorder "::" (reverse (calc/dna->digits calc/nmui-code dna)))))

(def opts-unreduced {:pre-simplify? false :reduce-dna? false})
(def opts-reduced {:pre-simplify? true :reduce-dna? true})

;; Note: by default, `=>*` adds extra reduction steps before & after the result
;;       while `==>*` does not (to keep a predictable number of output terms)
(deftest eval->x-all-test
  (testing "Correct output shape, given various options" ;; all verified
    (is (= :i
           (=>* [:u])))
    (is (= [:i]
           (==>* [:u])))
    (is (= :x
           (=>* :x)))
    (is (= [:_]
           (==>* :x)))
    (is (= :_
           (=>* :x {} {:allow-value-holes? true})))
    (is (= '(a :g)
           (=>* ['a :g])))
    (is (= '[:fdna [a] [:_ :_ :_ :n]]
           (=>* ['a :g] {} {:allow-value-holes? true})))
    (is (= [:_ :_ :_ :n]
           (==>* ['a :g])))
    (is (= [[:x]]
           (=>* [[:x]] {} {:pre-simplify? false})))
    (is (= :x
           (=>* [[:x]] {} {:pre-simplify? true})))
    (is (= :m
           (=>* [:- [] ['b ['b]]])))
    (is (= '[:fdna [b] [:m :m :m :m]]
           (=>* [:- [] ['b ['b]]] {} opts-unreduced)))
    (is (= [:m :m :m :m]
           (==>* [:- [] ['b ['b]]])))
    (is (= [:m]
           (==>* [:- [] ['b ['b]]] {} opts-reduced)))

    (is (= '((a (:x (b))))
           (=>* [:- 'a [:x ['b]]])))
    (is (= '[:fdna [a b] [:n :_ :_ :_ :u :u :_ :_ :i :_ :i :_ :m :m :m :m]]
           (=>* [:- 'a [:x ['b]]] {} {:allow-value-holes? true})))
    (is (= [:n :_ :_ :_ :u :u :_ :_ :i :_ :i :_ :m :m :m :m]
           (==>* [:- 'a [:x ['b]]])))

    (is (= '[:fdna [a] [:n :u :i :m]]
           (=>* [:- 'a ['b ['b]]])
           (=>* [:- 'a ['b ['b]]] {} {:pre-simplify? false})
           (=>* [:- 'a ['b ['b]]] {} {:reduce-dna? false})))
    (is (= '[:fdna [a b] [:n :n :n :n :u :u :u :u :i :i :i :i :m :m :m :m]]
           (=>* [:- 'a ['b ['b]]] {} opts-unreduced)))
    (is (= [:n :u :i :m]
           (==>* [:- 'a ['b ['b]]] {} opts-reduced)
           (==>* [:- 'a ['b ['b]]] {} {:pre-simplify? true})
           (==>* [:- 'a ['b ['b]]] {} {:reduce-dna? true})))
    (is (= [:n :n :n :n :u :u :u :u :i :i :i :i :m :m :m :m]
           (==>* [:- 'a ['b ['b]]])))

    (is (= '[:fdna [a c] [:n :u :i :m :u :u :m :m :i :m :i :m :m :m :m :m]]
           (=>* [:- 'a ['b ['b]] 'c])))
    (is (= [:n :u :i :m  :n :u :i :m  :n :u :i :m  :n :u :i :m
            :u :u :m :m  :u :u :m :m  :u :u :m :m  :u :u :m :m
            :i :m :i :m  :i :m :i :m  :i :m :i :m  :i :m :i :m
            :m :m :m :m  :m :m :m :m  :m :m :m :m  :m :m :m :m]
           (==>* [:- 'a ['b ['b]] 'c])))
    (is (= [:n :u :i :m  :u :u :m :m  :i :m :i :m  :m :m :m :m]
           (==>* [:- 'a ['b ['b]] 'c] {} opts-reduced)))
    )
  
  (testing "Correctness of returned combinatorial space"
    (is (= :n
           (=>* (make nil))
           (=>* (make nil) {} opts-unreduced)))
    (is (= [:n]
           (==>* (make nil))
           (==>* (make nil) {} opts-unreduced)))

    (is (= [:fdna '(a) [:n :u :i :m]]
           (=>* (make 'a))
           (=>* (make 'a) {} opts-unreduced)))

    (is (= [:n :u :i :m]
           (==>* (make 'a))
           (==>* (make 'a) {} opts-unreduced)))

    (is (= [:fdna '(a b) [:n :u :i :m :u :u :m :m :i :m :i :m :m :m :m :m]]
           (=>* (make 'a 'b))
           (=>* (make 'a 'b) {} opts-unreduced)))

    (is (= [:n :u :i :m :u :u :m :m :i :m :i :m :m :m :m :m]
           (==>* (make 'a 'b) {} opts-unreduced)))

    (is (= [:fdna '(a b c)
            [:n :u :i :m  :u :u :m :m  :i :m :i :m  :m :m :m :m
             :u :u :m :m  :u :u :m :m  :m :m :m :m  :m :m :m :m
             :i :m :i :m  :m :m :m :m  :i :m :i :m  :m :m :m :m
             :m :m :m :m  :m :m :m :m  :m :m :m :m  :m :m :m :m]]
           (=>* (make 'a 'b 'c)))))

  (testing "Correctness of evaluation for simple seq-re FORMs"
    (is (= '[:fdna [a] [:i :i :i :n]]
           (=>* (seq-re :<r 'a)) (=>* (seq-re :<..r. 'a))))
    (is (= '[:fdna [a] [:u :u :u :n]]
           (=>* (seq-re :<..r 'a))))

    (is (= '[:fdna [a] [:i :u :u :m]]
           (=>* (seq-re :<r_ 'a)) (=>* (seq-re :<..r._ 'a))))
    (is (= '[:fdna [a] [:u :i :i :m]]
           (=>* (seq-re :<..r_ 'a))))

    (is (= '[:fdna [a] [:i :i :i :n]]
           (=>* (seq-re :<r' 'a)) (=>* (seq-re :<..r'. 'a))))
    (is (= '[:fdna [a] [:u :u :u :n]]
           (=>* (seq-re :<..r' 'a))))

    (is (= '[:fdna [a] [:i :m :i :m]]
           (=>* (seq-re :<r'_ 'a)) (=>* (seq-re :<..r'._ 'a))))
    (is (= '[:fdna [a] [:u :u :m :m]]
           (=>* (seq-re :<..r'_ 'a)))))

  (testing "Correctness of evaluation in complex seq-re FORMs"
    (is (= '[:fdna [a b] [:u :i :u :i :i :i :i :i :i :n :i :n :n :n :n :n]]
           (=>* (seq-re :<r 'b [:- :i 'a] 'a)))))

  (testing "Congruence of evaluated formDNA with formform 1 results"
    ;; SelFi Collection (see https://observablehq.com/@formsandlines/1d-ca-for-4-valued-form-logic-selfis)

    ;; Mark1
    (is (= (->nmui (=>* (make (seq-re :<r 'l 'e 'r)
                              (seq-re :<r 'l 'r 'e))
                        {} {:varorder ['l 'e 'r]}))
           "[l e r]::3121103223011213012313312301311301231032230132103121133123011113"))

    ;; StripesD100000
    (is (= (->nmui (=>* (make (seq-re :<r 'l 'e 'r))
                        {} {:varorder ['l 'e 'r]}))
           "[l e r]::3302200223013003030323022301030303032002230100003302230223013303"))

    ;; StripesL000100
    (is (= (->nmui (=>* (make (seq-re :<r 'l 'r 'e))
                        {} {:varorder ['l 'e 'r]}))
           "[l e r]::3223303000002213022033330000321302203030000032103223333300002213"))

    ;; Mono000101
    (is (= (->nmui (=>* (make (seq-re :<r 'l 'r 'e)
                              (seq-re :<r 'e 'l 'r))
                        {} {:varorder ['l 'e 'r]}))
           "[l e r]::3121333303031111222213312002111121211331230111113223333300001113"))

    ;; Rhythm101101
    (is (= (->nmui (=>* (make (seq-re :<r 'l 'e 'r)
                              (seq-re :<r 'e 'r 'l)
                              (seq-re :<r 'l 'r 'e)
                              (seq-re :<r 'e 'l 'r))
                        {} {:varorder ['l 'e 'r]}))
           "[l e r]::3121111121211111111113311331111121211331230111111111111111111113"))

    ;; NewSense
    (is (= (->nmui (=>* (make (seq-re :<r 'l 'e 'r)
                              (seq-re :<r 'r 'e 'l)
                              (seq-re :<r 'l 'r 'e))
                        {} {:varorder ['l 'e 'r]}))
           "[l e r]::3121121221211213311313311331311301231032230132101111111111111113"))

    ;; Slit / xor4vRnd
    (is (= (->nmui (=>* (make '((l) r) '((r) l))))
           "[l r]::0123103223013210"))

    ;; or4v
    (is (= (->nmui (=>* (make 'l 'r)))
           "[l r]::3113121211113210"))

    ;; xorReId / xorReIdRnd
    (is (= (->nmui (=>* (make (seq-re :<r' 'l 'r) (seq-re :<r' 'r 'l))))
           "[l r]::2121123223011212"))

    ;; Rule4v30
    (is (= (->nmui (=>* (make '((l) e r)
                              '((e) l) '((r) l))
                        {} {:varorder ['l 'e 'r]}))
           "[l e r]::0220212122220123133130303333103220020303000023013113121211113210"))

    ;; Rule4v111
    (is (= (->nmui (=>* (make '(((l) e) r)
                              '(((l) r) e)
                              '(((e) r) l))
                        {} {:varorder ['l 'e 'r]}))
           "[l e r]::2121121221211212311313311331311301231032230132101111111111111111"))

    ;; Structure111Re / Co(mprehend)OneAnother (identical to “NewSense”)
    (is (= (->nmui (=>* (make (seq-re :<r 'l 'e 'r)
                              (seq-re :<r 'e 'r 'l)
                              (seq-re :<r 'l 'r 'e))
                        {} {:varorder ['l 'e 'r]}))
           "[l e r]::3121121221211213311313311331311301231032230132101111111111111113"))

    ;; Rule4v110
    (is (= (->nmui (=>* (make '((e) r)
                              '((r) e)
                              '((r) l))
                        {} {:varorder ['l 'e 'r]}))
           "[l e r]::0123121221213210311310321331321001231032230132103113121211113210"))

    ;; uniTuringReRnd
    (is (= (->nmui (=>* (form (form (seq-re :<r 'l 'e 'r)
                                    (seq-re :<r 'e 'r 'l)
                                    (seq-re :<r 'l 'r 'e))
                              (form 'l 'e 'r))
                        {} {:varorder ['l 'e 'r]}))
           "[l e r]::3123121221213213311310321331321001231032230132103113121211113210"))))


(deftest eval-tsds-test
  (testing "Correct output shape"
    (is (= [:i :m :m :m :m :m :m :m :m :m :m :m :m :m :m :m
            :m :m :m :m :i :i :i :i :m :m :m :m :i :i :i :i
            :m :m :m :m :m :m :m :m :i :u :i :u :u :u :u :u
            :m :m :m :m :i :i :i :i :u :u :u :u :n :n :n :n]
           (ts==>* 0 1 1 0 1 0)))))



(deftest evaluate-test
  (testing "Correct output shape"
    (is (submap? '{:result :n, :simplified nil}
                 (evaluate [['x] [:n]])
                 (evaluate [['x] ['a]] {'a :n})))
    (is (submap? '{:result :i, :simplified [:u] :env {}}
                 (evaluate (form (seq-re :<r nil nil)))))
    (is (submap? '{:result nil :simplified ((x) (:x)) :env {a :x}}
                 (evaluate [['x] ['a]] {'a :x})))
    (is (submap? '{:result nil :simplified ((x) (:x))}
                 (evaluate [['x] [:x]])))
    (is (submap? '{:result nil :simplified x}
                 (evaluate [['x] [:m]])
                 (evaluate [['x] ['a]] {'a :m})))
    (is (submap? '{:result nil :simplified ((x) (a))}
                 (evaluate [['x] ['a]])
                 (evaluate [:- [['x] ['a]] [[:seq-re :<r nil nil] 'a [:u]]])))))


(deftest eval-all-test
  (testing "Correct output shape"
    (is (submap? '{:varorder [], :results [[[] :n]]}
                 (eval-all [[:u] :u])))
    (is (submap? '{:varorder ["apple"],
                   :results [[[:n] :n] [[:u] :n] [[:i] :i] [[:m] :i]]}
                 (eval-all [["apple"] :u])))
    (is (submap? '{:varorder [a x],
                   :results ;; verified in FORM tricorder v1
                   [[[:n :n] :i] [[:n :u] :i] [[:n :i] :i] [[:n :m] :i]
                    [[:u :n] :i] [[:u :u] :m] [[:u :i] :i] [[:u :m] :m]
                    [[:i :n] :n] [[:i :u] :n] [[:i :i] :i] [[:i :m] :i]
                    [[:m :n] :n] [[:m :u] :u] [[:m :i] :i] [[:m :m] :m]]}
                 (eval-all [:- [['x] ['a]] [:u 'a]])))
    (is (submap? '{:varorder [a b],
                   :results
                   [[[:n :n] :n ] [[:n :u] nil] [[:n :i] nil] [[:n :m] nil]
                    [[:u :n] :u ] [[:u :u] :u ] [[:u :i] nil] [[:u :m] nil]
                    [[:i :n] :i ] [[:i :u] nil] [[:i :i] :i ] [[:i :m] nil]
                    [[:m :n] :m ] [[:m :u] :m ] [[:m :i] :m ] [[:m :m] :m ]]}
                 (eval-all [:- 'a [:x ['b]]])))
    (is (submap? '{:varorder [a b],
                   :results
                   [[[:n :n] :n] [[:n :u] :_] [[:n :i] :_] [[:n :m] :_]
                    [[:u :n] :u] [[:u :u] :u] [[:u :i] :_] [[:u :m] :_]
                    [[:i :n] :i] [[:i :u] :_] [[:i :i] :i] [[:i :m] :_]
                    [[:m :n] :m] [[:m :u] :m] [[:m :i] :m] [[:m :m] :m]]}
                 (eval-all [:- 'a [:x ['b]]] {} {:allow-value-holes? true})))
    (is (submap? '{:varorder [a],
                   :results
                   [[[:n] {:result nil, :simplified (:g),      :env {a :n}}],
                    [[:u] {:result nil, :simplified (:u :g),   :env {a :u}}],
                    [[:i] {:result nil, :simplified ([:u] :g), :env {a :i}}],
                    [[:m] {:result :n,  :simplified nil,       :env {a :m}}]]}
                 (eval-all ['a :g] {} {:rich-results? true})))
    (is (submap? '{:varorder [a],
                   :results
                   [[[:n] {:result :_, :simplified (:g),      :env {a :n}}],
                    [[:u] {:result :_, :simplified (:u :g),   :env {a :u}}],
                    [[:i] {:result :_, :simplified ([:u] :g), :env {a :i}}],
                    [[:m] {:result :n, :simplified nil,       :env {a :m}}]]}
                 (eval-all ['a :g] {} {:rich-results? true
                                       :allow-value-holes? true})))

    (is (submap? '{:varorder [a b],
                   :results
                   [[[:n :n] :n] [[:n :u] :n] [[:n :i] :n] [[:n :m] :n]
                    [[:u :n] :u] [[:u :u] :u] [[:u :i] :u] [[:u :m] :u]
                    [[:i :n] :i] [[:i :u] :i] [[:i :i] :i] [[:i :m] :i]
                    [[:m :n] :m] [[:m :u] :m] [[:m :i] :m] [[:m :m] :m]]}
                 (eval-all [:- 'a ['b ['b]]])))
    (is (submap? '{:varorder [a],
                   :results [[[:n] :n] [[:u] :u] [[:i] :i] [[:m] :m]]}
                 (eval-all [:- 'a ['b ['b]]] {} {:pre-simplify? true})))))


(deftest interpret-test
  (testing "Equality of alternative operations"
    (is (= '[[:- "a"] [[]] :u]
           (form {:splice? false} [:- "a"] [[]] :u)
           (make {:mark? true :splice? false} [:- "a"] [[]] :u)
           (interpret (make :+ [:- "a"] [[]] :u))))
    (is (= '[[[:- "a"] [[]] :u]]
           (interpret-walk {:--focus #{:+}} (form :+ [:- "a"] [[]] :u))
           (interpret (make {:splice? false} [:- "a"] [[]] :u))
           (interpret (make :- [:- "a"] [[]] :u))))
    (is (= '[[["a"] [[[]]] [:u]]]
           (interpret-walk {:--focus #{:|}} (form :| [:- "a"] [[]] :u))
           (interpret
            (interpret (make :* [:- "a"] [[]] :u)))))
    (is (= '[["a"] [[[]]] [:u]]
           (core/splice-ctx
            (interpret-walk {:--focus #{:*}} (form :* [:- "a"] [[]] :u)))
           (interpret (make :| [:- "a"] [[]] :u)))))

  (testing "Correctness of transformation"
    (is (= (interpret [:- 'x 'y])
           '((x y))))
    (is (= (interpret [:mem [['a :m] ['b :u]] [:- 'x 'y]])
           '[[[a :m] [[a] [:m]]] [[b :u] [[b] [:u]]] [x y]]))
    (is (= (interpret 'a)
           'a))
    (is (= (interpret {'a :m} 'a)
           :m))
    (is (= (interpret {'a :m} '(a (b (a))))
           '(a (b (a)))))
    (is (= (interpret [:* 'a [:* 'b 'c]])
           '[:- [a] [[:* b c]]]))
    (is (= (interpret [:- :u [:- 'a [:i]]])
           '[[:u [:- a [:i]]]])))
  (testing "--defocus flags"
    (is (= (interpret {:--defocus #{:ops}} [:uncl "hey"])
           [:uncl "hey"]))))

(deftest interpret*-test
  (testing "Correctness of transformation"
    (is (= (interpret* [:* 'a 'b 'c])
           '[[[a] [b] [c]]]))
    (is (= (interpret* {'a :m} 'a)
           []))
    (is (= (interpret* [:* 'a [:* 'b 'c]])
           '[[[a] [[:* b c]]]]))
    (is (= (interpret* [:- :u [:- 'a [:i]]])
           '[[:u [:- a [:i]]]]))))

(deftest interpret-walk-test
  (testing "Correctness of transformation"
    (is (= (interpret-walk {'a :m} '(a (b (a))))
           '(:m (b (:m)))))
    ;; is it okay to splice expressions in interpret?
    (is (= (interpret-walk [:* 'a [:* 'b 'c]])
           '[:- [a] [[b] [c]]]))
    (is (= (interpret-walk [:- :u [:- 'a [:i]]])
           '[[[:seq-re :<r nil nil] [[a [[:u]]]]]]))))

(deftest interpret-walk*-test
  (testing "Correctness of transformation"
    (is (= (interpret-walk* {'a :m} '(a (b (a))))
           '([] (b ([])))))
    (is (= (interpret-walk* [:* 'a [:* 'b 'c]])
           '[[[a] [[b] [c]]]]))
    (is (= (interpret-walk* [:- :u [:- 'a [:i]]])
           '[[[[[:f* [[:f*]]] [[:f*] [[[:f*]]]]] [:f*]]
              [[a [[[[[:f* [[:f*]]] [[:f*] [[[:f*]]]]] [:f*]]]]]]]])))

  (testing "--defocus flags"
    (is (= (interpret-walk* {:--defocus #{:ops}} [:- :u [:- 'a [:i]]])
           '[:- [:seq-re :<r nil nil] [:- a [[[:seq-re :<r nil nil]]]]]))
    (is (= (interpret-walk* {:--defocus #{:mem}} [:- :u [:- 'a [:i]]])
           '[[[:mem [[:f* [[:f*]]]] :f*]
              [[a [[[:mem [[:f* [[:f*]]]] :f*]]]]]]]))))




(deftest simplify-op-test
  (testing "formDNA"
    (testing "Basic functionality"
      (is (= (simplify-op (make :fdna [] [:n]) {}) :n))
      (is (= (simplify-op (make :fdna ['a] [:n :u :i :m]) {'a :u}) :u))
      (is (= (simplify-op (make :fdna ['a 'b] [:n :u :i :m
                                               :u :i :m :i
                                               :i :m :i :u
                                               :m :i :u :n]) {'a :m})
             '[:fdna [b] [:m :i :u :n]])))

    (testing "Partial matches in dictionary"
      (is (= (simplify-op (make :fdna ['a] [:n :u :i :m]) {'x :m})
             '[:fdna [a] [:n :u :i :m]]))
      (is (= (simplify-op (make :fdna ['a] [:n :u :i :m]) {'x :m 'a :u})
             :u)))

    (testing "Correctness of transformations"
      (are [x y] (= (simplify-op (make :fdna ['a] [:m :i :u :n]) {'a x}) y)
        :n :m
        :u :i
        :i :u
        :m :n)

      (is (= (simplify-op
              (make :fdna ['a 'b] [:n :u :i :m
                                   :u :i :m :i
                                   :i :m :i :u
                                   :m :i :u :n]) {'a :u})
             '[:fdna [b] [:u :i :m :i]]))))

  (testing "unclear FORMs"
    (testing "Basic functionality"
      (is (= (simplify-op (make :uncl "hey") {})
             '[:fdna ["hey"] [:u :u :u :n]]))

      (are [x y] (= (simplify-op (make :uncl "unkFo") {"unkFo" x}) y)
        :n :u
        :u :u
        :i :u
        :m :n)

      ;; !! this is correct, but what happened exactly?
      (is (= (>> (make :uncl "hey") {"hey" :m})
             nil))))

  (comment
    (simplify-op [:mem [['a []]] 'a] {}))

  (testing "memory FORMs"
    (testing "Correctness of reduction"
      (is (= (simplify-op (make :mem [['a :m]] 'a) {})
             (simplify-op (make :mem [['a []]] 'a) {})
             (simplify-op (make :mem [['a (form)]] 'a) {})
             (simplify-op (make :mem [['a (make :- [])]] 'a) {})
             '()))
      (is (= (simplify-op (make :mem [['a :u]] ['b] 'a) {})
             '[:- (b) :u]))
      (is (= (simplify-op (make :mem [['x :n]] 'y) {})
             'y)))

    ;; ? move to simplify-in-test
    (testing "In expression context"
      (is (= (in>> [(make :mem [['a :u]] ['a] 'b)])
             (in>> [(make :mem [['a :u]] ['a]) 'b])
             '[(:u) b]))
      (is (= (in>> [(make :mem [['a :u]] 'a) 'b])
             '[:u b]))
      ;; env substitution has priority over degeneration from observed values
      (is (= (in>> ['y (make :mem [['y 'z]] 'y)])
             '[y z]))
      ;; rems can shadow previous rems or given reduction env
      (is (= (in>> ['x (make :mem [['y 'z]] 'x)] {'x 'y})
             (in>> ['x (make :mem [['x 'z]] 'x)] {'x 'y})
             '[y z]))
      (is (= (in>> [(make :mem [['y 'x]] 'x)] {'x 'y})
             (in>> [(make :mem [['y [:- 'x]]] 'x)] {'x 'y})
             '[y]))
      (is (= (in>> [(make :mem [['x 'y] ['y 'z]] 'w)] {'w 'x})
             (in>> [(make :mem [['y 'z] ['x 'y]] 'w)] {'w 'x})
             '[z]))
      (is (= (in>> [(make :mem [['x [:- 'x 'z]]] 'x)] {'x 'y})
             '[y z]))
      (is (= (in>> ['x (make :mem [['y 'x]] 'x)] {'x 'y})
             '[y])))

    (testing "Substitution of values from recursive rems"
      ;; (= ctx' ctx) because reduce-by-calling:
      (is (= (simplify-op (make :mem [[:x [:- :x]]] :x) {})
             :x))
      ;; ? merge context during repeated substitution or only once afterwards?
      ;; !! too deeply nested
      #_(is (= (simplify-op (make :mem [[:x (make 'a [:- :x])]] :x) {})
               '[:mem [[:x [a :x]]] a :x])))

    (testing "Exception in infinite reduction (stack overflow)"
      ;; infinite recursion because outer expr env nullifies previous dissoc:
      (is (thrown-with-msg? Exception #"Context too deeply nested"
                            (simplify-op (make :mem [[:x [:x]]] :x) {})))
      (is (thrown-with-msg? Exception #"Context too deeply nested"
                            (simplify-op (make :mem [[:x [:- [:x]]]] :x) {})))
      (is (thrown-with-msg? Exception #"Context too deeply nested"
                            (simplify-op (make :mem [[:x [[:- :x]]]] :x) {}))))

    (testing "Combined with outer env"
      (= (simplify-op (make :mem [['a :n]] 'a 'b) {'b :u})
         :u))

    (testing "Reduction of shadowed rems"
      (is (= (simplify-op (make :mem [['a '(x)] ['b [:- 'a :u]]] [:- 'b]) {})
             '[:- (x) :u])))))


(deftest interpret-sym-test
  (testing "Unknown symbols"
    (is (= :x (interpret :x))))
  (testing "Value symbols"
    (are [x y] (= y (interpret x))
      :n nil
      :m '()
      :u (seq-re :<r nil nil)
      :i '(:u))))

(def seqre-opts
  (vec (for [interpr [:rec-instr :rec-ident]
             open?   [false true]
             parity  [:any :even :odd]]
         {:parity parity, :open? open?, :interpr interpr})))

(deftest interpret-op-test
  (testing "Syntactic operators"
    (testing "Relations"
      (is (= '[a b c]
             (interpret-op (make :+ 'a 'b 'c))))
      (is (= '[[a b c]]
             (interpret-op (make :- 'a 'b 'c))))
      (is (= '[:- [a] [b] [c]]
             (interpret-op (make :* 'a 'b 'c))
             (interpret-op (make :<> 'a 'b 'c))))
      (is (= '[[a] [b] [c]]
             (interpret-op (make :| 'a 'b 'c))
             (interpret-op (make :<-> 'a 'b 'c)))))

    (testing "Nestings"
      (is (= '[[[a] b] c]
             (interpret-op (make :<- 'a 'b 'c))))
      (is (= '[a [b [c]]]
             (interpret-op (make :-> 'a 'b 'c))))
      (is (= '[:- [[a] b] c]
             (interpret-op (make :< 'a 'b 'c))))
      (is (= '[:- a [b [c]]]
             (interpret-op (make :> 'a 'b 'c))))))
  
  ;; interpret :fdna returns arbitrary order because of `calc/dna->vdict`
  ;; therefore don’t depend on order of its results in test
  (testing "formDNA"
    (testing "Correctness of transformation"
      (is (= (interpret-op (make :fdna)) :n))
      (is (= (interpret-op (make :fdna [:u])) :u))
      (is (= (interpret-op (make :fdna [] [:m])) :m))
      (is (= (set (op-get (interpret-op (make :fdna ['a] [:n :u :i :m]))
                          :exprs))
             '#{[[:n] [[:n->m a]]]
                [[:u] [[:u->m a]]]
                [[:i] [[:i->m a]]]
                [[:m] [[:m->m a]]]}))
      (is (= (set (op-get (interpret-op (make :fdna ['a 'b] [:m :i :m :m
                                                             :i :n :m :u
                                                             :n :i :m :u
                                                             :i :n :i :m]))
                          :exprs))
             '#{[[:i] [[:m->m a]] [[:n->m b]]]
                [[:i] [[:m->m a]] [[:i->m b]]]
                [[:u] [[:u->m a]] [[:m->m b]]]
                [[:m] [[:m->m a]] [[:m->m b]]]
                [[:m] [[:n->m a]] [[:m->m b]]]
                [[:m] [[:n->m a]] [[:n->m b]]]
                [[:m] [[:u->m a]] [[:i->m b]]]
                [[:u] [[:i->m a]] [[:m->m b]]]
                [[:n] [[:u->m a]] [[:u->m b]]]
                [[:m] [[:n->m a]] [[:i->m b]]]
                [[:m] [[:i->m a]] [[:i->m b]]]
                [[:n] [[:i->m a]] [[:n->m b]]]
                [[:n] [[:m->m a]] [[:u->m b]]]
                [[:i] [[:u->m a]] [[:n->m b]]]
                [[:i] [[:i->m a]] [[:u->m b]]]
                [[:i] [[:n->m a]] [[:u->m b]]]}))))

  (testing "unclear FORMs"
    (testing "Correctness of transformation"
      (is (= (interpret-op (make :uncl "foo"))
             [:seq-re :<r "foo" "foo"]))
      (is (= (interpret-op (make :uncl (str #{:X :Y} #".+")))
             [:seq-re :<r "#{:Y :X}.+" "#{:Y :X}.+"]))))

  (testing "memory FORMs"
    (testing "Correctness of transformation"
      (is (= (interpret-op (make :mem [['a :m] ['b :u]] 'x 'y))
             '(((a :m) ((a) (:m))) ((b :u) ((b) (:u))) (x y))))))

  (let [f (fn f [opts & nested-exprs]
            (interpret-op (apply seq-re opts nested-exprs)))]
    (testing "self-equivalent re-entry FORMs"
      (testing "Shape of empty expression"
        (are [x y] (= x y)
          (f (seqre-opts 0))  '[:mem [[:f* ((:f*))] [:f1 (:f*)]] :f1]
          (f (seqre-opts 1))  '[:mem [[:f* ((:f*))]] :f*]
          (f (seqre-opts 2))  '[:mem [[:f* ((:f*))] [:f1 (:f*)]] :f1]
          (f (seqre-opts 3))  '[:mem [[:f* ((:f*))] [:f2 (:f*)] [:f1 :f2]] :f1]
          (f (seqre-opts 4))  '[:mem [[:f* ((:f*))] [:f1 :f*]] :f1]
          (f (seqre-opts 5))  '[:mem [[:f* ((:f*))] [:f2 (:f*)] [:f1 :f2]] :f1]

          (f (seqre-opts 6))  '[:mem [[:f* ((:f*))] [:f1 (:f*)]] :f1]
          (f (seqre-opts 7))  '[:mem [[:f* ((:f*))]] :f*]
          (f (seqre-opts 8))  '[:mem [[:f* ((:f*))] [:f1 (:f*)]] :f1]
          (f (seqre-opts 9))  '[:mem [[:f* ((:f*))] [:f2 (:f*)] [:f1 :f2]] :f1]
          (f (seqre-opts 10)) '[:mem [[:f* ((:f*))] [:f1 :f*]] :f1]
          (f (seqre-opts 11)) '[:mem [[:f* ((:f*))] [:f2 (:f*)] [:f1 :f2]] :f1]))

      (testing "Shape of expressions with resolution 1"
        (are [x y] (= x y)
          (f (seqre-opts 0) 'a)  '[:mem [[:f* ((:f* a) a)] [:f1 (:f* a)]] :f1]
          (f (seqre-opts 1) 'a)  '[:mem [[:f* ((:f* a) a)]] :f*]
          (f (seqre-opts 2) 'a)  '[:mem [[:f* ((:f* a) a)] [:f1 (:f* a)]] :f1]
          (f (seqre-opts 3) 'a)  '[:mem [[:f* ((:f* a) a)] [:f2 (:f* a)] [:f1 [:- :f2 a]]] :f1]
          (f (seqre-opts 4) 'a)  '[:mem [[:f* ((:f* a) a)] [:f1 [:- :f* a]]] :f1]
          (f (seqre-opts 5) 'a)  '[:mem [[:f* ((:f* a) a)] [:f2 (:f* a)] [:f1 [:- :f2 a]]] :f1]

          (f (seqre-opts 6) 'a)  '[:mem [[:f* ((:f* a) a)] [:f1 (:f* a)]] :f1]
          (f (seqre-opts 7) 'a)  '[:mem [[:f* ((:f* a) a)]] :f*]
          (f (seqre-opts 8) 'a)  '[:mem [[:f* ((:f* a) a)] [:f1 (:f* a)]] :f1]
          (f (seqre-opts 9) 'a)  '[:mem [[:f* ((:f* a) a)] [:f2 (:f* a)] [:f1 [:- :f2 a]]] :f1]
          (f (seqre-opts 10) 'a) '[:mem [[:f* ((:f* a) a)] [:f1 [:- :f* a]]] :f1]
          (f (seqre-opts 11) 'a) '[:mem [[:f* ((:f* a) a)] [:f2 (:f* a)] [:f1 [:- :f2 a]]] :f1]))

      (testing "Shape of expressions with even resolution"
        (are [x y] (= x y)
          ;; ? should even/odd construct redundant re-entries?
          (f (seqre-opts 0) 'a 'b)  '[:mem [[:f* ((:f* a) b)]] :f*]
          (f (seqre-opts 1) 'a 'b)  '[:mem [[:f* ((:f* a) b)]] :f*]
          (f (seqre-opts 2) 'a 'b)  '[:mem [[:f* ((:f* a) b)]] :f*]
          (f (seqre-opts 3) 'a 'b)  '[:mem [[:f* ((:f* a) b)] [:f1 [:- (:f* a) b]]] :f1]
          (f (seqre-opts 4) 'a 'b)  '[:mem [[:f* ((:f* a) b)] [:f1 [:- (:f* a) b]]] :f1]
          (f (seqre-opts 5) 'a 'b)  '[:mem [[:f* ((:f* a) b)] [:f1 [:- (:f* a) b]]] :f1]

          (f (seqre-opts 6) 'a 'b)  '[:mem [[:f* ((:f* a) b)]] :f*]
          (f (seqre-opts 7) 'a 'b)  '[:mem [[:f* ((:f* a) b)]] :f*]
          (f (seqre-opts 8) 'a 'b)  '[:mem [[:f* ((:f* a) b)]] :f*]
          (f (seqre-opts 9) 'a 'b)  '[:mem [[:f* ((:f* a) b)] [:f1 [:- (:f* a) b]]] :f1]
          (f (seqre-opts 10) 'a 'b) '[:mem [[:f* ((:f* a) b)] [:f1 [:- (:f* a) b]]] :f1]
          (f (seqre-opts 11) 'a 'b) '[:mem [[:f* ((:f* a) b)] [:f1 [:- (:f* a) b]]] :f1]))

      (testing "Shape of expressions with odd resolution"
        (are [x y] (= x y)
          (f (seqre-opts 0) 'a 'b 'c)  '[:mem [[:f* ((((((:f* a) b) c) a) b) c)] [:f1 (((:f* a) b) c)]] :f1]
          (f (seqre-opts 1) 'a 'b 'c)  '[:mem [[:f* ((((((:f* a) b) c) a) b) c)]] :f*]
          (f (seqre-opts 2) 'a 'b 'c)  '[:mem [[:f* ((((((:f* a) b) c) a) b) c)] [:f1 (((:f* a) b) c)]] :f1]
          (f (seqre-opts 3) 'a 'b 'c)  '[:mem [[:f* ((((((:f* a) b) c) a) b) c)] [:f2 (((:f* a) b) c)] [:f1 [:- ((:f2 a) b) c]]] :f1]
          (f (seqre-opts 4) 'a 'b 'c)  '[:mem [[:f* ((((((:f* a) b) c) a) b) c)] [:f1 [:- ((:f* a) b) c]]] :f1]
          (f (seqre-opts 5) 'a 'b 'c)  '[:mem [[:f* ((((((:f* a) b) c) a) b) c)] [:f2 (((:f* a) b) c)] [:f1 [:- ((:f2 a) b) c]]] :f1]

          (f (seqre-opts 6) 'a 'b 'c)  '[:mem [[:f* ((((((:f* a) b) c) a) b) c)] [:f1 (((:f* a) b) c)]] :f1]
          (f (seqre-opts 7) 'a 'b 'c)  '[:mem [[:f* ((((((:f* a) b) c) a) b) c)]] :f*]
          (f (seqre-opts 8) 'a 'b 'c)  '[:mem [[:f* ((((((:f* a) b) c) a) b) c)] [:f1 (((:f* a) b) c)]] :f1]
          (f (seqre-opts 9) 'a 'b 'c)  '[:mem [[:f* ((((((:f* a) b) c) a) b) c)] [:f2 (((:f* a) b) c)] [:f1 [:- ((:f2 a) b) c]]] :f1]
          (f (seqre-opts 10) 'a 'b 'c) '[:mem [[:f* ((((((:f* a) b) c) a) b) c)] [:f1 [:- ((:f* a) b) c]]] :f1]
          (f (seqre-opts 11) 'a 'b 'c) '[:mem [[:f* ((((((:f* a) b) c) a) b) c)] [:f2 (((:f* a) b) c)] [:f1 [:- ((:f2 a) b) c]]] :f1]))

      (testing "Arbitrary expressions"
        (is (= (interpret-op (seq-re :<..r_ nil 'x nil))
               '[:mem [[:f* ((((((:f*) x))) x))] [:f1 ((:f*) x)]] :f1]))))))


(deftest seq-reentry-opts->sign-test
  (testing "All possible inputs"
    (are [x y] (= x y)
      (seq-reentry-opts->sign (seqre-opts 0))  :<r
      (seq-reentry-opts->sign (seqre-opts 1))  :<..r
      (seq-reentry-opts->sign (seqre-opts 2))  :<..r.
      (seq-reentry-opts->sign (seqre-opts 3))  :<r_
      (seq-reentry-opts->sign (seqre-opts 4))  :<..r_
      (seq-reentry-opts->sign (seqre-opts 5))  :<..r._

      (seq-reentry-opts->sign (seqre-opts 6))  :<r'
      (seq-reentry-opts->sign (seqre-opts 7))  :<..r'
      (seq-reentry-opts->sign (seqre-opts 8))  :<..r'.
      (seq-reentry-opts->sign (seqre-opts 9))  :<r'_
      (seq-reentry-opts->sign (seqre-opts 10)) :<..r'_
      (seq-reentry-opts->sign (seqre-opts 11)) :<..r'._)))

(deftest seq-reentry-sign->opts-test
  (testing "Basic functionality"
    (is (= (seq-reentry-sign->opts :<r) (seqre-opts 0)))
    (is (= (seq-reentry-sign->opts :<..r'._) (seqre-opts 11)))))

(deftest seq-reentry-expr-test
  (testing "Empty expressions"
    (are [x y] (= x y)
      (seq-re (seqre-opts 0))  [:seq-re :<r nil]
      (seq-re (seqre-opts 1))  [:seq-re :<..r nil]
      (seq-re (seqre-opts 2))  [:seq-re :<..r. nil]
      (seq-re (seqre-opts 3))  [:seq-re :<r_ nil]
      (seq-re (seqre-opts 4))  [:seq-re :<..r_ nil]
      (seq-re (seqre-opts 5))  [:seq-re :<..r._ nil]

      (seq-re (seqre-opts 6))  [:seq-re :<r' nil]
      (seq-re (seqre-opts 7))  [:seq-re :<..r' nil]
      (seq-re (seqre-opts 8))  [:seq-re :<..r'. nil]
      (seq-re (seqre-opts 9))  [:seq-re :<r'_ nil]
      (seq-re (seqre-opts 10)) [:seq-re :<..r'_ nil]
      (seq-re (seqre-opts 11)) [:seq-re :<..r'._ nil]))

  (testing "Default type"
    (is (= (seq-re {}) '[:seq-re :<r nil])))

  (testing "Content number and type"
    (is (= (seq-re {} 'x) '[:seq-re :<r x]))
    (is (= (seq-re {} 'x 'y) '[:seq-re :<r x y]))
    (is (expression? (seq-re {})))
    (is (arrangement? (->> (seq-re {} (make 'x 'y) 'z)
                           op-data
                           :nested-exprs
                           first)))))


(deftest nested-test
  (testing "Shape of simple cases"
    (is (= (form-nested-l nil)
           '()))
    (is (= (form-nested-l nil nil)
           '(())))
    (is (= (form-nested-l 'a)
           '(a)))
    (is (= (form-nested-l 'a 'b)
           '((a) b)))
    (is (= (form-nested-l 'a 'b 'c)
           '(((a) b) c)))
    (is (= (make-nested-l nil)
           nil))
    (is (= (make-nested-l nil nil)
           '()))
    (is (= (make-nested-l 'a)
           'a))
    (is (= (make-nested-l 'a 'b)
           '[:- (a) b]))
    (is (= (make-nested-l 'a 'b 'c)
           '[:- ((a) b) c]))

    (is (= (form-nested-r nil)
           '()))
    (is (= (form-nested-r nil nil)
           '(())))
    (is (= (form-nested-r 'a)
           '(a)))
    (is (= (form-nested-r 'a 'b)
           '(a (b))))
    (is (= (form-nested-r 'a 'b 'c)
           '(a (b (c)))))
    (is (= (make-nested-r nil)
           nil))
    (is (= (make-nested-r nil nil)
           '()))
    (is (= (make-nested-r 'a)
           'a))
    (is (= (make-nested-r 'a 'b)
           '[:- a (b)]))
    (is (= (make-nested-r 'a 'b 'c)
           '[:- a (b (c))])))

  (testing "Shape of output expression"
    (is (= (form-nested-l [:- :f 'a] nil 'b nil)
           '((((:f a)) b))))
    (is (= (form-nested-r [:- :f 'a] nil 'b nil)
           '(:f a ((b ())))))))


;; ? collect in make tests
(deftest n->m-test
  (is (= (interpret-op (make :n->m 'a))
         '[[:seq-re :<r (a)] [:seq-re :<..r (a)]])))

(deftest m->m-test
  (is (= (interpret-op (make :m->m 'a))
         '[[:seq-re :<r a] [:seq-re :<..r a]])))

(deftest u->m-test
  (is (= (interpret-op (make :u->m 'a))
         '[([:seq-re :<r (a)] a) ([:seq-re :<..r a] (a))])))

(deftest i->m-test
  (is (= (interpret-op (make :i->m 'a))
         '[([:seq-re :<r a] (a)) ([:seq-re :<..r (a)] a)])))

(deftest selector-test
  ;; !! unverified
  (testing "Correct expression"
    (is (= (selector {})
           '()))
    (is (= (selector {'a :m})
           '((a))))
    (is (= (selector {'a :i})
           '[:- ((a) ([:seq-re :<..r nil])) (a ([:seq-re :<r nil]))]))
    (is (= (selector {'a :m 'b :n 'c :m})
           '((a) b (c))))
    (is (= (selector {'a :m, 'b :n, 'c :u})
           '[:- (a (b) c ([:seq-re :<..r nil])) (a (b) (c) ([:seq-re :<r nil]))])))

  (testing "Without simplification"
    (is (= (selector {} false)
           '[:- (([:seq-re :<..r nil])) (([:seq-re :<r nil]))]))
    (is (= (interpret-op (selector {'a :m} false))
           '[[(a ([:seq-re :<..r nil])) (a ([:seq-re :<r nil]))]]))))


(deftest equal?-test
  (testing "Trivial cases"
    (are [x] (equal? x x)
      nil [] [:- 'a 'b] [['a] :u] (seq-re :<r "x" "y"))
    (is (equal? nil [[]] [[] []]))
    (is (equal? [] [nil] [[[]]] [[[] []]] [:- [] []])))


  (testing "Non-equality with redundant variables"
    (is (not (equal? '[[a] a] :n)))
    (is (not (equal? '[[p] p] nil)))
    (is (not (equal? '[:- b [[a] a]] 'b)))
    (is (not (equal? (seq-re :<r '(a (a)) 'b '(c (c)))
                     (seq-re :<r nil 'b nil)))))


  (testing "Non-equality with different variable names"
    (is (equal? '[:- a a] 'a))
    (is (not (equal? '[:- a a] 'x))))


  (testing "Non-equality with missing variables"
    (is (not (equal? '[[p] p] nil))))


  (testing "Non-equality with swapped variable names"
    (is (equal? [['a] 'b] ['b ['a]]))
    (is (not (equal? [['a] 'b] [['b] 'a]))))


  (testing "Equality of algebraic transformations"
    (is (equal? '[:- a a] 'a))
    (is (not (equal? '[:- a a] 'b)))
    (is (equal? '[a a] '[a]))
    (is (equal? '[[a]] 'a [:- 'a]))
    (is (not (equal? '[["a"]] 'a)))
    (is (equal? '[a] '[[[a]]]))
    (is (equal? '[:- [[a] [b]] [a b]] '[[[a] b] [[b] a]]))
    (is (equal? '[[[a] [b]] [a b]] '[:- [[a] b] [[b] a]]))
    (is (equal? '[[p r] [q r]] '[:- [[p] [q]] r]))))



(deftest equiv?-test
  (testing "Trivial cases"
    (are [x] (equiv? x x)
      nil [] [:- 'a 'b] [['a] :u] (seq-re :<r "x" "y"))
    (is (equiv? nil [[]] [[] []]))
    (is (equiv? [] [nil] [[[]]] [[[] []]] [:- [] []])))


  (testing "Equivalence with redundant variables"
    (is (equiv? '[[a] a] :n nil [[]] '(a (((a a))) a) [:- '(a nil (a :n))]))
    (is (equiv? '[:- b [[a] a]] 'b))
    (is (equiv? [:- "b" [["a"] "a"]] "b"))
    (is (equiv? (seq-re :<r '(a (a)) 'b '(c (c))) (seq-re :<r nil 'b nil))))


  (testing "Equivalence with swapped variable names"
    (is (equiv? [['a] 'b] [['b] 'a])))


  (testing "Equivalence despite missing variables"
    (is (equiv? '[[p] p] nil))
    (is (not (equiv? '[[p] q] nil))))


  (testing "Equivalence despite different variable names"
    (is (equiv? '[:- a a] 'x))
    (is (equiv? '["apple" "apple"] '[a]))
    (is (equiv? '[[x]] 'a [:- "foo bar"]))
    (is (equiv? '[boo] '[[[a]]]))
    (is (equiv? '[:- [[a] [b]] [a b]] '[[[x] y] [[y] x]]))
    (is (equiv? '[[["me"] ["you"]] ["me" "you"]] '[:- [[a] b] [[b] a]]))
    (is (equiv? '[[p r] [q r]] '[:- [[x] [xx]] xxx])))


  (testing "Equivalence in algebraic demonstrations"
    ;; LoF, prim. Alg.
    ;; C1
    (is (equiv? '((a))
                '[:- (((a)) (a)) ((a))]
                '((((a)) (a)) (((a)) a))
                '((((a)) a))
                '((((a)) a) ((a) a))
                '[:- ((((a))) ((a))) a]
                'a))
    ;; C2
    (is (equiv? '[:- (a b) b]
                '[:- (((a)) b) b]
                '[:- (((a)) ((b))) b]
                '(((a) b) ((b) b))
                '(((a) b))
                '[:- (a) b]))
    ;; C3
    (is (equiv? '[:- () a]
                '[:- (a) a]
                '(((a) a))
                '()))
    ;; C4
    (is (equiv? '[:- ((a) b) a]
                '[:- ((a) b a) a]
                '[:- ((a b) b a) a]
                'a))
    ;; C5
    (is (equiv? '[:- a a]
                '[:- ((a)) a]
                'a))
    ;; C6
    (is (equiv? '[:- ((a) (b)) ((a) b)]
                '((((a) (b)) ((a) b)))
                '((((b)) (b)) (a))
                '((a))
                'a))
    ;; C7
    (is (equiv? '(((a) b) c)
                '(((a) ((b))) c)
                '(((a c) ((b) c)))
                '[:- (a c) ((b) c)]))
    ;; C8
    (is (equiv? '((a) (b r) (c r))
                '((a) (((b r) (c r))))
                '((a) (((b) (c)) r))
                '[:- ((a) (b) (c)) ((a) (r))]))
    ;; C9 (commented out because slow, but test passes)
    ;; (is (equiv? '(((b) (r)) ((a) (r)) ((x) r) ((y) r))
    ;;            '(((b) (r)) ((a) (r)) ((x y) r))
    ;;            '[:- (b a ((x y) r)) (r ((x y) r))]
    ;;            '[:- (b a ((x y) r)) (r x y)]
    ;;            '[:- (b a ((x y) r) (r x y)) (r x y)]
    ;;            '[:- ((r) a b) (r x y)]))

    ;; uFORM iFORM (engl. Ed.)
    ;; p.7
    (is (equiv? '((a) b)
                '[:- ((a) b) ((a) b)]
                '[:- ((a) ((b))) ((a) b)]
                '((((a) b) a) (((a) b) (b)))
                '((((a) b) a) ((b)))
                '((((a) b) a) b)
                '((((a) b) ((a) b) a) b)
                '((((a) ((b))) ((a) b) a) b)
                '((((((a) b) a) (((a) b) (b))) a) b)
                '((((((a) b) a) ((b))) a) b)
                '((((((a) b) a) b) a) b)))
    ;; p.9
    (is (equiv? (seq-re :<r :m :m) (seq-re :<r :n :m) :n nil))
    (is (equiv? (seq-re :<r :m :n) :m '()))
    (is (equiv? (seq-re :<r nil nil) :u))
    ;; p.10
    (is (equiv? [(seq-re :<r :m :m)] [(seq-re :<r :n :m)] [:n] '()))
    (is (equiv? [(seq-re :<r :m :n)] [:m] '(()) nil))
    (is (equiv? [(seq-re :<r nil nil)] [:u] :i))
    ;; p.11
    (is (equiv? [:- (seq-re :<r nil nil) '()] [:- :u '()]
                [:- [(seq-re :<r nil nil)] '()] [:- :i '()]
                '()))
    (is (equiv? (seq-re :<r [:- (seq-re :<r 'a 'b) '()] 'c)
                (seq-re :<r '() 'c)
                '((()) c)
                '(c)))
    (is (equiv? (seq-re :<r 'a [:- (seq-re :<r 'b 'c) '()])
                (seq-re :<r 'a '())
                '(()) nil))
    ;; p.17
    (is (equiv? (make :uncl "uncFo") (seq-re :<r "uncFo" "uncFo")))
    (is (equiv? (seq-re :<r '() '()) '(()) nil))
    (is (equiv? (seq-re :<r '(()) '(())) :u))))


(deftest permute-vars-test
  (testing "Correct permutations"
    (is (= (permute-vars '[a b c])
           '[[a b c]
             [a c b]
             [b a c]
             [b c a]
             [c a b]
             [c b a]])))) 

(deftest formDNA-perspectives-test
  (testing "Correct perspectives"
    (let [dna [:n :n :n :n :n :n :n :n :n :n :n :n :n :n :n :n :u :u :u :u :n :u :n :u :u :u :u :u :n :u :n :u :i :i :i :i :i :i :i :i :n :n :i :i :n :n :i :i :m :m :m :m :i :m :i :m :u :u :m :m :n :u :i :m]]
      (is (= (formDNA-perspectives (make :fdna '[a b c] dna))
             (apply make
                    (map (fn [varorder [_ dna]]
                           (make :fdna varorder dna))
                         '[[a b c] [a c b] [b a c] [b c a] [c a b] [c b a]]
                         (calc/dna-perspectives dna))))))))

(deftest tsds-op-test
  (testing "Correct re-entry selection in interpretation of tsds"
    (is (= '[:-]
           (interpret (make :tsds [0 0 0 0 0 0] 'l 'e 'r))))
    (is (= '[:-
             [:seq-re :<r l e r] [:seq-re :<r r e l] [:seq-re :<r e r l]
             [:seq-re :<r l r e] [:seq-re :<r r l e] [:seq-re :<r e l r]]
           (interpret (make :tsds [1 1 1 1 1 1] 'l 'e 'r))))
    (is (= '[:- [:seq-re :<r r e l] [:seq-re :<r e r l] [:seq-re :<r r l e]]
           (interpret (make :tsds [0 1 1 0 1 0] 'l 'e 'r)))))
  (testing "Consistent expressions in interpretation of tsds"
    (is (= '[:-
             [:seq-re :<r "apple" [:- x y] (((a) b) c)]
             [:seq-re :<r [:- x y] "apple" (((a) b) c)]]
           (interpret (make :tsds [1 0 0 0 0 1]
                            "apple" [:- 'x 'y] '(((a) b) c)))))))

