(ns formform.expr-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [formform.calc :as calc]
            [formform.expr.core :as core]
            [formform.expr :refer :all]
            [orchestra.spec.test :as stest]))

(def rv (comp vec reverse))

(doseq [fsym fns-with-specs] (stest/instrument fsym))


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
    (is (= '[:mem [[x :U]] x] (make-op :mem [['x :U]] 'x)))
    (is (= '[:seq-re :<r a b] (make-op :seq-re :<r 'a 'b))))

  ;; ! needs validation and tests
  (testing "Invalid arguments to known operators"
    (is (= true true))))

(deftest valid-op?-test
  (testing "Context of the test assertions"
    (is (true? (valid-op? (make :fdna ['a 'b]
                                [:M :I :U :N
                                 :I :M :N :U
                                 :U :N :M :I
                                 :N :U :I :M]))))
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
    (is (= '{:rems [[x :U]], :ctx [x]}
           (op-data (make-op :mem [['x :U]] 'x))))
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
    (let [op (make-op :mem [['x :U]] 'x)]
      (is (= '[[x :U]] (op-get op :rems)))
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
           (find-vars :U {})
           (find-vars (make) {})
           (find-vars (form) {'a :M})
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
           (find-vars (make :fdna ['x "y"] [:N :U :I :M
                                            :N :U :I :M
                                            :N :U :I :M
                                            :N :U :I :M]) {})))
    (is (= '(a x y "b" "z")
           (find-vars (make :mem [['a '(x y)] ["b" "z"]] 'a) {})))
    (is (= '("foo")
           (find-vars (make :uncl "foo") {})))
    (is (= '(a b c)
           (find-vars (seq-re :<r [:- 'a 'b] 'c) {}))))

  (testing "In nested special FORMs"
    (is (= '(a b c)
           (find-vars (seq-re :<r [:- 'a :M] [:- '(b) (seq-re :<..r 'c)]) {})))
    (is (= '(a b c d)
           (find-vars (make :mem [['a (make :mem [['b :U]] 'c)]] 'd) {}))))

  (testing "Differentiated from other elements"
    (is (= '(a)
           (find-vars (make :N 'a nil) {}))))

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



(defn- simplify-expr-chain-reversed [r->l? exprs env]
  (let [rev-exprs (fn [xs] (reverse (map #(if (arrangement? %)
                                           (cons (first %) (reverse (rest %)))
                                           %) xs)))]
    (rev-exprs (chain>> {:rtl? r->l?} (rev-exprs exprs) {}))))

(def chain>>-rtl (partial simplify-expr-chain-reversed true))
(def chain>>-ltr (partial simplify-expr-chain-reversed false))

(comment
  ;; test
  (let [xc '[ a nil b]]
    {:normal (chain>> xc {})
     :rtl (chain>>-rtl xc {})
     :reverse (reverse (chain>> (reverse xc) {}))})

  (chain>> '( nil ) {}))


;; ! add more tests with marks ()
(deftest simplify-expr-chain-test
  (testing "Correctness of reduction with one form per nesting-level"
    (are [x y] (= y (chain>> x {}) (chain>>-rtl x {}))
      '( nil ) '[nil] ;; () => ()
      '( () )  '[()]  ;; (()) => (())
      '( a )   '[a]   ;; (a) => (a)

      '( nil nil ) '[()] ;; (())
      '( a nil )   '[()] ;; (a ()) => (())
      '( nil a )   '[nil a] ;; ((a))
      '( () () )   '[()] ;; (() (())) => (())
      '( a () )    '[a] ;; (a (())) => (a)
      '( () a )    '[()] ;; (() (a)) => (())
      '( a b )     '[a b] ;; (a (b))
      '( a a )     '[()] ;; (a (a)) => (a ()) => (())

      '( nil nil nil ) '[nil] ;; ((())) => ()
      '( a nil nil )   '[a] ;; (a)
      '( a nil () )    '[()] ;; (a ((()))) => (a ()) => (())
      '( nil a nil )   '[nil] ;; ((a ())) => ((())) => ()
      '( nil a () )    '[nil a] ;; ((a (()))) => ((a))
      '( nil nil a )   '[a] ;; (((a))) => (a)
      '( nil nil () )  '[()] ;; (((()))) => (())
      '( nil a b )     '[nil a b] ;; ((a (b)))
      '( a nil b )     '[[:- a b]] ;; (a ((b))) => (a b)
      '( a b nil )     '[a] ;; (a (b ())) => (a (())) => (a)
      '( a b c )       '[a b c] ;; (a (b (c)))

      '( nil nil nil nil ) '[()] ;; (((()))) => (())
      '( a b nil nil )     '[a b] ;; (a (b (()))) => (a (b))
      '( a nil nil b )     '[a b] ;; (a (((b)))) => (a (b))
      '( nil nil a b )     '[a b] ;; (((a (b)))) => (a (b))
      '( a nil b c )       '[[:- a b] c] ;; (a ((b (c)))) => (a b (c))
      '( a b nil c )       '[a [:- b c]] ;; (a (b ((c)))) => (a (b c))

      '( a nil b nil c )   '[[:- a b c]] ;; (a ((b ((c))))) => (a b c)
      '( a nil nil b c )   '[a b c] ;; (a (((b (c))))) => (a (b (c)))
      '( a b nil nil c )   '[a b c] ;; (a (b (((c))))) => (a (b (c)))
      '( a nil nil nil b ) '[[:- a b]] ;; (a ((((b))))) => (a b)
      '( nil a nil b nil ) '[nil] ;; ((a ((b ())))) => ((a b ())) => … => ()
      '( nil nil nil a b ) '[nil a b] ;; ((((a (b))))) => ((a (b)))
      '( a b nil nil nil ) '[a])) ;; (a (b ((())))) => (a (b ())) => … => (a)

  (testing "Equal reduction behavior with forms wrapped in arrangements"
    (are [x y] (= y (chain>> x {}) (chain>>-rtl x {}))
      '( [:- nil] ) '[nil]
      '( [:- ()] )  '[()]
      '( [:- a] )   '[a]

      '( [:- nil] [:- nil] ) '[()]
      '( [:- nil] nil )      '[()]
      '( nil [:- nil] )      '[()]
      '( [:- a] [:- nil] )   '[()]
      '( [:- a] nil )        '[()]
      '( a [:- nil] )        '[()]
      '( [:- nil] [:- a] )   '[nil a]
      '( [:- nil] a )        '[nil a]
      '( nil [:- a] )        '[nil a]
      '( [:- ()] [:- ()] )   '[()]
      '( [:- ()] () )        '[()]
      '( () [:- ()] )        '[()]
      '( [:- a] [:- ()] )    '[a]
      '( [:- a] () )         '[a]
      '( a [:- ()] )         '[a]
      '( [:- ()] [:- a] )    '[()]
      '( [:- ()] a )         '[()]
      '( () [:- a] )         '[()]
      '( [:- a] [:- b] )     '[a b]
      '( [:- a] b )          '[a b]
      '( a [:- b] )          '[a b]
      '( [:- a] [:- a] )     '[()]
      '( [:- a] a )          '[()]
      '( a [:- a] )          '[()]))

  (testing "Correctness of reduction with multiple forms per nesting-level"
    (are [x y] (= y (chain>> x {}) (chain>>-rtl x {}))
      '( [:- nil nil] ) '[nil]
      '( [:- () ()] )   '[()]
      '( [:- a a] )     '[a]

      '( [:- nil a] [:- nil ()] ) '[a] ;; (a (())) => (a)
      '( [:- a ()] [:- nil b] )   '[()])) ;; (a () (b)) => (())

  (testing "Correctness of reduction via (de)generation rule across chain"
    (let [x [[:- 'a 'b] [:- 'a 'c]]]
      (is (= '[[:- a b] c]
             (chain>> x {}) (chain>>-rtl x {}))))
    ;; (a b (a c)) => (a b (c))
    (let [x ['x 'a nil 'b 'x]]
      (is (= '[x]
             (chain>> x {}) (chain>>-rtl x {}))))
    ;; (x (a ( (b (x))))) => (x (a b ())) => (x)
    (let [x [[:- 'a 'b] [:- 'a 'c] [:- 'c 'a 'd]]]
      (is (= '[[:- a b] c d]
             (chain>> x {})
             (chain>>-rtl x {}))))
    ;; (a b (a c (c a d))) => (a b (c (d)))
    (let [x [[:- :U 'b] [:- 'a :I]]]
      (is (= '[[:- :U b]]
             (chain>> x {})
             (chain>>-rtl x {}))))
    ;; (:U b (a :I)) => (:U b (a :U :I)) => (:U b (())) => (:U b)
    (let [x [[:- :U 'b] [:- 'a :I] 'c]]
      (is (= '[[:- :U b]]
             (chain>> x {})
             (chain>>-rtl x {}))))
    ;; (:U b (a :I (c))) => (:U b (a () (c))) => (:U b (())) => (:U b)

    (let [x [:f* [:- :U 'a] [:- :U 'b]]]
      (is (= '[:f* a [:- :U b]]
             (chain>> {:rtl? true} x {})
             (chain>>-ltr x {}))))
    ;; (((:f*) :U a) :U b) => (((:f*) a) :U b)
    (let [x [:f* :U :U]]
      (is (= '[[:- :f* :U]]
             (chain>> {:rtl? true} x {})
             (chain>>-ltr x {}))))))
;; (((:f*) :U) :U) => (((:f*)) :U) => (:f* :U)


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
          (>> :N) nil
          (>> :M) '()
          (>> :U) :U
          (>> :I) '(:U)))
      (testing "marked"
        (are [x y] (= x y)
          (>> '(:N)) '()
          (>> '(:M)) nil
          (>> '(:U)) '(:U)
          (>> '(:I)) :U)))

    (testing "variable"
      (testing "without env"
        (is (= (>> 'a) 'a))
        (is (= (>> "a") "a")))

      (testing "with env"
        (is (= (>> 'a {'a :M})
               (>> '(a) {'a :N})
               (>> '((a (b))) {'a :I 'b :I})
               '()))
        (is (= (>> "a" {"a" :U}) :U)))))

  (testing "Nested content"
    (testing "reduced by calling"
      (is (= (>> '(nil () nil () nil))
             (>> '(() :M (:N)))
             nil))
      (is (= (>> '(:U nil :U :U))
             '(:U)))
      (is (= (>> '(:I nil :I (:U)))
             :U))
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
             (>> '(((:M))))
             (>> '((((nil) nil) nil) nil))
             nil))
      (is (= (>> '((())))
             (>> '(((nil))))
             (>> '(((((:N))))))
             (>> '(((((nil) nil) nil) nil) nil))
             '()))
      (is (= (>> '(((()) (()))))
             (>> '((((nil)) (:M))))
             (>> '(((:U)) ((:I))))
             nil))
      (is (= (>> '(((a))))
             '(a)))
      (is (= (>> '((a)))
             'a))
      (is (= (>> '((x)) {'x :U})
             :U))
      (is (= (>> '(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((x))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) {'x :U})
             '(:U))))

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
      (is (= (in>> [ :N :N ]) []))
      (is (= (in>> [ :M :M ]) [ '()]))
      (is (= (in>> [ :U :U ]) [ :U]))
      (is (= (in>> [ [:U] [:U] ]) [ '(:U)]))
      (is (= (in>> [ :I :I ]) [ '(:U)]))
      (is (= (in>> [ [:I] [:I] ]) [ :U]))
      (is (= (in>> [ :M :N ]) [ '()]))
      (is (= (in>> [ :U :N ]) [ :U]))
      (is (= (in>> [ :I :N ]) [ '(:U)]))
      (is (= (in>> [ :U :I ]) [ '()]))
      (is (= (in>> [ [:U] :I ]) [ '(:U)]))
      (is (= (in>> [ [:I] :U ]) [ :U]))
      (is (= (in>> [ [:U] [:I] ]) [ '()]))
      (is (= (in>> [ :U :M ]) [ '()]))
      (is (= (in>> [ :I :M ]) [ '()])))

    (testing "Variables"
      (testing "without env"
        (is (= (in>> [ 'a 'b])
               [ 'a 'b]))
        (is (= (in>> [ 'a 'a])
               [ 'a]))
        (is (= (in>> [ 'a "a"]) ;; should be equal?
               [ 'a "a"])))

      (testing "with env"
        (is (= (in>> [ 'a 'a ] {'a :U})
               [ :U]))
        (is (= (in>> [ 'a 'b ] {'a :U 'b :I})
               [ '()]))
        (is (= (in>> [ 'a "a" ] {'a :U "a" :I}) ;; should be equal?
               [ '()]))
        (is (= (in>> [ 'a 'b ] {'a :U})
               [ :U 'b]))
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
      (is (= (in>> [ '(()) nil :N '(((()))) '(() () ())])
             []))
      (is (= (in>> [ '((() ())) :M '()])
             [ '()]))
      (is (= (in>> [ :U '((:U)) :U '((:U) (:U))])
             [ :U]))
      (is (= (in>> [ '(:U) '(((:U))) :I '((:I) (:I))])
             [ '(:U)]))
      (is (= (in>> [ :U '(:I)])
             [ :U]))
      (is (= (in>> [ '(:I) '(:U)])
             [ '()]))
      (is (= (in>> [ '((:U)) '(:U)])
             [ '()]))
      (is (= (in>> [ :U '(:I)])
             [ :U]))
      (is (= (in>> [ '(:U) '((:U))])
             [ '()]))
      (is (= (in>> [ '((:I)) :U])
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
      (is (= (in>> '[ :U (:I)])
             [ :U]))
      (is (= (in>> '[ :U (:N (:I))])
             [ '()]))
      (is (= (in>> '[ :U (:N (:N (:I)))])
             [ :U]))
      (is (= (in>> '[ :U (:N (:U (:N (:I))))])
             [ '()]))
      (is (= (in>> '[ :N (:U (:N (:N (:I))))])
             [ '(:U)]))
      (is (= (in>> '[ :I (:U)])
             [ '(:U)]))
      (is (= (in>> '[ :I (:N (:U))])
             [ '()]))
      (is (= (in>> '[ :I (:N (:N (:U)))])
             [ '(:U)]))
      (is (= (in>> '[ :I (:N (:I (:N (:U))))])
             [ '()]))
      (is (= (in>> '[ :N (:I (:N (:N (:U))))])
             [ :U]))
      (is (= (in>> '[ :U (:I a)])
             [ :U]))
      (is (= (in>> '[ :U ((a :U) :I)])
             [ :U]))
      (is (= (in>> '[ :U ((a :I) :U)])
             [ '()]))
      (is (= (in>> '[ :I ((a (b :U (c))) c)])
             [ '(:U) '((a) c)]))))

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
    (is (= (in>> (repeat 100 :U)) [ :U]))
    (is (= (in>> (repeat 100 '(:U))) [ '(:U)]))
    (is (= (in>> (repeat 100 :N)) []))
    (is (= (in>> (repeat 100 :M)) [ '()]))
    (is (= (in>> (repeat 100 :U)) [ :U]))
    (is (= (in>> (repeat 100 :I)) [ '(:U)])))

  (testing "Operators in context"
    (is (= '[[:seq-re :<r a b]]
           (in>> [[:- [:seq-re :<r 'a 'b]]])))
    ;; law of crossing cannot be applied to (known) operators
    (is (= '[(a b c)]
           (in>> [ [[:- 'a 'b 'c]]])))
    (is (= '[:? y] ;; operator unknown
           (in>> [ [[:? 'y]]])))
    (is (= '[([:fdna ["foo"] [:U :U :U :N]])]
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
    (is (= '[:U]
           (in>> [[:mem [['x :U]] 'x] [:mem [['x :U]] 'x]])))
    (is (= '[[:fdna ["foo"] (:U :U :U :N)]]
           (in>> [[:uncl "foo"] [:uncl "foo"] [:uncl "foo"]])
           (in>> [[:fdna ["foo"] [:U :U :U :N]
                   [:fdna ["foo"] [:U :U :U :N]]]])))
    (is (= '[[:seq-re :<..r a b]]
           (in>> [[:seq-re :<..r 'a 'b] [:seq-re :<..r 'a 'b]])))
    ;; cannot cross re-entry boundary in (de)generation!
    (is (= '[[:seq-re :<r nil a] a]
           (in>> [[:seq-re :<r 'a 'a] 'a])
           (in>> [[:- [:seq-re :<r 'a 'a] 'a]])))
    (is (= '[[:seq-re :<r [:U] a] :U]
           (in>> [[:seq-re :<r :I 'a] :U])
           (in>> [[:seq-re :<r [:- :I] 'a] :U])))
    (is (= '[[:seq-re :<r a :U] [:U]]
           (in>> [[:seq-re :<r 'a :U] :I])
           (in>> [[:seq-re :<r 'a [:- :U]] :I])))
    ;; dominance of mark still holds
    (is (= '[[]]
           (in>> [[:seq-re :<r 'a 'b] []])
           (in>> [[] [:seq-re :<r 'a 'b]])
           (in>> [[] [[[:seq-re :<r 'a 'b]]]])
           (in>> [:U [:seq-re :<r 'a 'b] :I])))
    (is (= '[[]]
           (in>> [[:seq-re :<r nil nil] :I])
           (in>> [[:seq-re :<r nil] :U])
           (in>> [[:seq-re :<r nil nil] [:seq-re :<r nil]])))
    (is (= '[[]]
           (in>> [[:- [:seq-re :<r 'a 'b] []]])
           (in>> [[:fdna [] [:M]] [:seq-re :<r 'a 'b]])
           (in>> [[:fdna ["foo"] [:N :U :U :U]] [:fdna [] [:M]]])))))



(deftest eval->expr-test
  (testing "Reducable expressions"
    (testing "FORMs"
      (are [x y] (= x y)
        :N (=> (make nil))
        :M (=> (make '()))
        :U (=> (make :U))
        :I (=> (make '(:U)))))

    (testing "Constants"
      (are [x y] (= x y)
        :N (=> (make :N))
        :M (=> (make :M))
        :U (=> (make :U))
        :I (=> (make :I)))))

  (testing "irreducable expressions"
    (is (= :_ (=> (make 'a))))
    (is (= :_ (=> (make '("x" ("y"))))))))


(defn ->nmui [fdna-expr]
  (let [{:keys [varorder dna]} (op-data fdna-expr)]
    (apply str varorder "::" (reverse (calc/dna->digits calc/nmui-code dna)))))

(deftest eval->expr-all-test
  (testing "Correctness of returned combinatorial space"
    (is (= [:fdna '() [:N]]
           (=>* (make nil))))

    (is (= [:fdna '(a) [:N :U :I :M]]
           (=>* (make 'a))))

    (is (= [:fdna '(a b) [:N :U :I :M
                          :U :U :M :M
                          :I :M :I :M
                          :M :M :M :M]]
           (=>* (make 'a 'b))))

    (is (= [:fdna '(a b c)
            [:N :U :I :M  :U :U :M :M  :I :M :I :M  :M :M :M :M
             :U :U :M :M  :U :U :M :M  :M :M :M :M  :M :M :M :M
             :I :M :I :M  :M :M :M :M  :I :M :I :M  :M :M :M :M
             :M :M :M :M  :M :M :M :M  :M :M :M :M  :M :M :M :M]]
           (=>* (make 'a 'b 'c)))))

  (testing "Correctness of evaluation for simple seq-re FORMs"
    (is (= '[:fdna [a] [:I :I :I :N]]
           (=>* (seq-re :<r 'a)) (=>* (seq-re :<..r. 'a))))
    (is (= '[:fdna [a] [:U :U :U :N]]
           (=>* (seq-re :<..r 'a))))

    (is (= '[:fdna [a] [:I :U :U :M]]
           (=>* (seq-re :<r_ 'a)) (=>* (seq-re :<..r._ 'a))))
    (is (= '[:fdna [a] [:U :I :I :M]]
           (=>* (seq-re :<..r_ 'a))))

    (is (= '[:fdna [a] [:I :I :I :N]]
           (=>* (seq-re :<r' 'a)) (=>* (seq-re :<..r'. 'a))))
    (is (= '[:fdna [a] [:U :U :U :N]]
           (=>* (seq-re :<..r' 'a))))

    (is (= '[:fdna [a] [:I :M :I :M]]
           (=>* (seq-re :<r'_ 'a)) (=>* (seq-re :<..r'._ 'a))))
    (is (= '[:fdna [a] [:U :U :M :M]]
           (=>* (seq-re :<..r'_ 'a)))))

  (testing "Correctness of evaluation in complex seq-re FORMs"
    (is (= '[:fdna [a b] [:U :I :U :I :I :I :I :I :I :N :I :N :N :N :N :N]]
           (=>* (seq-re :<r 'b [:- :I 'a] 'a)))))

  (testing "Congruence of evaluated formDNA with formform 1 results"
    ;; SelFi Collection (see https://observablehq.com/@formsandlines/1d-ca-for-4-valued-form-logic-selfis)

    ;; Mark1
    (is (= (->nmui (=>* {:varorder ['l 'e 'r]}
                        (make (seq-re :<r 'l 'e 'r)
                              (seq-re :<r 'l 'r 'e)) {}))
           "[l e r]::3121103223011213012313312301311301231032230132103121133123011113"))

    ;; StripesD100000
    (is (= (->nmui (=>* {:varorder ['l 'e 'r]}
                        (make (seq-re :<r 'l 'e 'r)) {}))
           "[l e r]::3302200223013003030323022301030303032002230100003302230223013303"))

    ;; StripesL000100
    (is (= (->nmui (=>* {:varorder ['l 'e 'r]}
                        (make (seq-re :<r 'l 'r 'e)) {}))
           "[l e r]::3223303000002213022033330000321302203030000032103223333300002213"))

    ;; Mono000101
    (is (= (->nmui (=>* {:varorder ['l 'e 'r]}
                        (make (seq-re :<r 'l 'r 'e)
                              (seq-re :<r 'e 'l 'r)) {}))
           "[l e r]::3121333303031111222213312002111121211331230111113223333300001113"))

    ;; Rhythm101101
    (is (= (->nmui (=>* {:varorder ['l 'e 'r]}
                        (make (seq-re :<r 'l 'e 'r)
                              (seq-re :<r 'e 'r 'l)
                              (seq-re :<r 'l 'r 'e)
                              (seq-re :<r 'e 'l 'r)) {}))
           "[l e r]::3121111121211111111113311331111121211331230111111111111111111113"))

    ;; NewSense
    (is (= (->nmui (=>* {:varorder ['l 'e 'r]}
                        (make (seq-re :<r 'l 'e 'r)
                              (seq-re :<r 'r 'e 'l)
                              (seq-re :<r 'l 'r 'e)) {}))
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
    (is (= (->nmui (=>* {:varorder ['l 'e 'r]}
                        (make '((l) e r)
                              '((e) l) '((r) l)) {}))
           "[l e r]::0220212122220123133130303333103220020303000023013113121211113210"))

    ;; Rule4v111
    (is (= (->nmui (=>* {:varorder ['l 'e 'r]}
                        (make '(((l) e) r)
                              '(((l) r) e)
                              '(((e) r) l)) {}))
           "[l e r]::2121121221211212311313311331311301231032230132101111111111111111"))

    ;; Structure111Re / Co(mprehend)OneAnother (identical to “NewSense”)
    (is (= (->nmui (=>* {:varorder ['l 'e 'r]}
                        (make (seq-re :<r 'l 'e 'r)
                              (seq-re :<r 'e 'r 'l)
                              (seq-re :<r 'l 'r 'e)) {}))
           "[l e r]::3121121221211213311313311331311301231032230132101111111111111113"))

    ;; Rule4v110
    (is (= (->nmui (=>* {:varorder ['l 'e 'r]}
                        (make '((e) r)
                              '((r) e)
                              '((r) l)) {}))
           "[l e r]::0123121221213210311310321331321001231032230132103113121211113210"))

    ;; uniTuringReRnd
    (is (= (->nmui (=>* {:varorder ['l 'e 'r]}
                        (form (form (seq-re :<r 'l 'e 'r)
                                    (seq-re :<r 'e 'r 'l)
                                    (seq-re :<r 'l 'r 'e))
                              (form 'l 'e 'r)) {}))
           "[l e r]::3123121221213213311310321331321001231032230132103113121211113210"))))



(deftest evaluate-test
  (testing "Correct output format"
    (is (= '{:result :N}
           (evaluate [['x] [:N]])
           (evaluate [['x] ['a]] {'a :N})))
    (is (= '{:result x}
           (evaluate [['x] [:M]])
           (evaluate [['x] ['a]] {'a :M})))
    (is (= '{:result ((x) (a))}
           (evaluate [['x] ['a]])
           (evaluate [:- [['x] ['a]] [[:seq-re :<r nil nil] 'a [:U]]])))))

(deftest eval-all-test
  (testing "Correct output format"
    (is (= '{:varorder [], :results ([[] :N])}
           (eval-all [[:U] :U])))
    (is (= '{:varorder ["apple"],
             :results ([[:N] :N] [[:U] :N] [[:I] :I] [[:M] :I])}
           (eval-all [["apple"] :U])))
    (is (= '{:varorder [a x],
             :results ;; verified in FORM tricorder v1
             ([[:N :N] :I]
              [[:N :U] :I]
              [[:N :I] :I]
              [[:N :M] :I]
              [[:U :N] :I]
              [[:U :U] :M]
              [[:U :I] :I]
              [[:U :M] :M]
              [[:I :N] :N]
              [[:I :U] :N]
              [[:I :I] :I]
              [[:I :M] :I]
              [[:M :N] :N]
              [[:M :U] :U]
              [[:M :I] :I]
              [[:M :M] :M])}
           (eval-all [:- [['x] ['a]] [:U 'a]])))))


(deftest interpret-test
  (testing "Equality of alternative operations"
    (is (= '[[:- "a"] [[]] :U]
           (form {:splice? false} [:- "a"] [[]] :U)
           (make {:mark? true :splice? false} [:- "a"] [[]] :U)
           (interpret (make :+ [:- "a"] [[]] :U))))
    (is (= '[[[:- "a"] [[]] :U]]
           (interpret-walk {:--focus #{:+}} (form :+ [:- "a"] [[]] :U))
           (interpret (make {:splice? false} [:- "a"] [[]] :U))
           (interpret (make :- [:- "a"] [[]] :U))))
    (is (= '[[["a"] [[[]]] [:U]]]
           (interpret-walk {:--focus #{:|}} (form :| [:- "a"] [[]] :U))
           (interpret
            (interpret (make :* [:- "a"] [[]] :U)))))
    (is (= '[["a"] [[[]]] [:U]]
           (core/splice-ctx
            (interpret-walk {:--focus #{:*}} (form :* [:- "a"] [[]] :U)))
           (interpret (make :| [:- "a"] [[]] :U)))))

  (testing "Correctness of transformation"
    (is (= (interpret [:- 'x 'y])
           '((x y))))
    (is (= (interpret [:mem [['a :M] ['b :U]] [:- 'x 'y]])
           '[[[a :M] [[a] [:M]]] [[b :U] [[b] [:U]]] [x y]]))
    (is (= (interpret 'a)
           'a))
    (is (= (interpret {'a :M} 'a)
           :M))
    (is (= (interpret {'a :M} '(a (b (a))))
           '(a (b (a)))))
    (is (= (interpret [:* 'a [:* 'b 'c]])
           '[:- [a] [[:* b c]]]))
    (is (= (interpret [:- :U [:- 'a [:I]]])
           '[[:U [:- a [:I]]]])))
  (testing "--defocus flags"
    (is (= (interpret {:--defocus #{:ops}} [:uncl "hey"])
           [:uncl "hey"]))))

(deftest interpret*-test
  (testing "Correctness of transformation"
    (is (= (interpret* [:* 'a 'b 'c])
           '[[[a] [b] [c]]]))
    (is (= (interpret* {'a :M} 'a)
           []))
    (is (= (interpret* [:* 'a [:* 'b 'c]])
           '[[[a] [[:* b c]]]]))
    (is (= (interpret* [:- :U [:- 'a [:I]]])
           '[[:U [:- a [:I]]]]))))

(deftest interpret-walk-test
  (testing "Correctness of transformation"
    (is (= (interpret-walk {'a :M} '(a (b (a))))
           '(:M (b (:M)))))
    ;; is it okay to splice expressions in interpret?
    (is (= (interpret-walk [:* 'a [:* 'b 'c]])
           '[:- [a] [[b] [c]]]))
    (is (= (interpret-walk [:- :U [:- 'a [:I]]])
           '[[[:seq-re :<r nil nil] [[a [[:U]]]]]]))))

(deftest interpret-walk*-test
  (testing "Correctness of transformation"
    (is (= (interpret-walk* {'a :M} '(a (b (a))))
           '([] (b ([])))))
    (is (= (interpret-walk* [:* 'a [:* 'b 'c]])
           '[[[a] [[b] [c]]]]))
    (is (= (interpret-walk* [:- :U [:- 'a [:I]]])
           '[[[[[:f* [[:f*]]] [[:f*] [[[:f*]]]]] [:f*]]
              [[a [[[[[:f* [[:f*]]] [[:f*] [[[:f*]]]]] [:f*]]]]]]]])))

  (testing "--defocus flags"
    (is (= (interpret-walk* {:--defocus #{:ops}} [:- :U [:- 'a [:I]]])
           '[:- [:seq-re :<r nil nil] [:- a [[[:seq-re :<r nil nil]]]]]))
    (is (= (interpret-walk* {:--defocus #{:mem}} [:- :U [:- 'a [:I]]])
           '[[[:mem [[:f* [[:f*]]]] :f*]
              [[a [[[:mem [[:f* [[:f*]]]] :f*]]]]]]]))))




(deftest simplify-op-test
  (testing "formDNA"
    (testing "Basic functionality"
      (is (= (simplify-op (make :fdna [] [:N]) {}) :N))
      (is (= (simplify-op (make :fdna ['a] [:N :U :I :M]) {'a :U}) :U))
      (is (= (simplify-op (make :fdna ['a 'b] [:N :U :I :M
                                               :U :I :M :I
                                               :I :M :I :U
                                               :M :I :U :N]) {'a :M})
             '[:fdna [b] [:M :I :U :N]])))

    (testing "Partial matches in dictionary"
      (is (= (simplify-op (make :fdna ['a] [:N :U :I :M]) {'x :M})
             '[:fdna [a] [:N :U :I :M]]))
      (is (= (simplify-op (make :fdna ['a] [:N :U :I :M]) {'x :M 'a :U})
             :U)))

    (testing "Correctness of transformations"
      (are [x y] (= (simplify-op (make :fdna ['a] [:M :I :U :N]) {'a x}) y)
        :N :M
        :U :I
        :I :U
        :M :N)

      (is (= (simplify-op
              (make :fdna ['a 'b] [:N :U :I :M
                                   :U :I :M :I
                                   :I :M :I :U
                                   :M :I :U :N]) {'a :U})
             '[:fdna [b] [:U :I :M :I]]))))

  (testing "unclear FORMs"
    (testing "Basic functionality"
      (is (= (simplify-op (make :uncl "hey") {})
             '[:fdna ["hey"] [:U :U :U :N]]))

      (are [x y] (= (simplify-op (make :uncl "unkFo") {"unkFo" x}) y)
        :N :U
        :U :U
        :I :U
        :M :N)

      ;; !! this is correct, but what happened exactly?
      (is (= (>> (make :uncl "hey") {"hey" :M})
             nil))))

  (comment
    (simplify-op [:mem [['a []]] 'a] {}))

  (testing "memory FORMs"
    (testing "Correctness of reduction"
      (is (= (simplify-op (make :mem [['a :M]] 'a) {})
             (simplify-op (make :mem [['a []]] 'a) {})
             (simplify-op (make :mem [['a (form)]] 'a) {})
             (simplify-op (make :mem [['a (make :- [])]] 'a) {})
             '()))
      (is (= (simplify-op (make :mem [['a :U]] ['b] 'a) {})
             '[:- (b) :U]))
      (is (= (simplify-op (make :mem [['x :N]] 'y) {})
             'y)))

    ;; ? move to simplify-in-test
    (testing "In expression context"
      (is (= (in>> [(make :mem [['a :U]] ['a] 'b)])
             (in>> [(make :mem [['a :U]] ['a]) 'b])
             '[(:U) b]))
      (is (= (in>> [(make :mem [['a :U]] 'a) 'b])
             '[:U b]))
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
      (= (simplify-op (make :mem [['a :N]] 'a 'b) {'b :U})
         :U))

    (testing "Reduction of shadowed rems"
      (is (= (simplify-op (make :mem [['a '(x)] ['b [:- 'a :U]]] [:- 'b]) {})
             '[:- (x) :U])))))


(deftest interpret-sym-test
  (testing "Unknown symbols"
    (is (= :x (interpret :x))))
  (testing "Value symbols"
    (are [x y] (= y (interpret x))
      :N nil
      :M '()
      :U (seq-re :<r nil nil)
      :I '(:U))))

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

  (testing "formDNA"
    (testing "Correctness of transformation"
      (is (= (interpret-op (make :fdna)) :N))
      (is (= (interpret-op (make :fdna [:U])) :U))
      (is (= (interpret-op (make :fdna [] [:M])) :M))
      (is (= (interpret-op (make :fdna ['a] [:N :U :I :M]))
             '[:-
               [[:N] [[:N->M a]]]
               [[:U] [[:U->M a]]]
               [[:I] [[:I->M a]]]
               [[:M] [[:M->M a]]]]))
      (is (= (interpret-op (make :fdna ['a 'b] [:M :I :M :M
                                                :I :N :M :U
                                                :N :I :M :U
                                                :I :N :I :M]))
             '[:-
               [[:I] [[:M->M a]] [[:N->M b]]]
               [[:I] [[:M->M a]] [[:I->M b]]]
               [[:U] [[:U->M a]] [[:M->M b]]]
               [[:M] [[:M->M a]] [[:M->M b]]]
               [[:M] [[:N->M a]] [[:M->M b]]]
               [[:M] [[:N->M a]] [[:N->M b]]]
               [[:M] [[:U->M a]] [[:I->M b]]]
               [[:U] [[:I->M a]] [[:M->M b]]]
               [[:N] [[:U->M a]] [[:U->M b]]]
               [[:M] [[:N->M a]] [[:I->M b]]]
               [[:M] [[:I->M a]] [[:I->M b]]]
               [[:N] [[:I->M a]] [[:N->M b]]]
               [[:N] [[:M->M a]] [[:U->M b]]]
               [[:I] [[:U->M a]] [[:N->M b]]]
               [[:I] [[:I->M a]] [[:U->M b]]]
               [[:I] [[:N->M a]] [[:U->M b]]]]))))

  (testing "unclear FORMs"
    (testing "Correctness of transformation"
      (is (= (interpret-op (make :uncl "foo"))
             [:seq-re :<r "foo" "foo"]))
      (is (= (interpret-op (make :uncl (str #{:X :Y} #".+")))
             [:seq-re :<r "#{:Y :X}.+" "#{:Y :X}.+"]))))

  (testing "memory FORMs"
    (testing "Correctness of transformation"
      (is (= (interpret-op (make :mem [['a :M] ['b :U]] 'x 'y))
             '(((a :M) ((a) (:M))) ((b :U) ((b) (:U))) (x y))))))

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


(deftest nest-exprs-test
  (testing "Shape of simple cases"
    (is (= (nest-exprs {} nil)
           '()))
    (is (= (nest-exprs {} nil nil)
           '(())))
    (is (= (nest-exprs {} 'a)
           '(a)))
    (is (= (nest-exprs {} 'a 'b)
           '((a) b)))
    (is (= (nest-exprs {} 'a 'b 'c)
           '(((a) b) c)))
    (is (= (nest-exprs {:unmarked? true} nil)
           nil))
    (is (= (nest-exprs {:unmarked? true} nil nil)
           '()))
    (is (= (nest-exprs {:unmarked? true} 'a)
           'a))
    (is (= (nest-exprs {:unmarked? true} 'a 'b)
           '[:- (a) b]))
    (is (= (nest-exprs {:unmarked? true} 'a 'b 'c)
           '[:- ((a) b) c]))

    (is (= (nest-exprs {:ltr? true} nil)
           '()))
    (is (= (nest-exprs {:ltr? true} nil nil)
           '(())))
    (is (= (nest-exprs {:ltr? true} 'a)
           '(a)))
    (is (= (nest-exprs {:ltr? true} 'a 'b)
           '(a (b))))
    (is (= (nest-exprs {:ltr? true} 'a 'b 'c)
           '(a (b (c)))))
    (is (= (nest-exprs {:ltr? true :unmarked? true} nil)
           nil))
    (is (= (nest-exprs {:ltr? true :unmarked? true} nil nil)
           '()))
    (is (= (nest-exprs {:ltr? true :unmarked? true} 'a)
           'a))
    (is (= (nest-exprs {:ltr? true :unmarked? true} 'a 'b)
           '[:- a (b)]))
    (is (= (nest-exprs {:ltr? true :unmarked? true} 'a 'b 'c)
           '[:- a (b (c))])))

  (testing "Shape of output expression"
    (is (= (nest-exprs {} [:- :f 'a] nil 'b nil)
           '((((:f a)) b))))))


;; ? collect in make tests
(deftest N->M-test
  (is (= (interpret-op (make :N->M 'a))
         '[[:seq-re :<r (a)] [:seq-re :<..r (a)]])))

(deftest M->M-test
  (is (= (interpret-op (make :M->M 'a))
         '[[:seq-re :<r a] [:seq-re :<..r a]])))

(deftest U->M-test
  (is (= (interpret-op (make :U->M 'a))
         '[([:seq-re :<r (a)] a) ([:seq-re :<..r a] (a))])))

(deftest I->M-test
  (is (= (interpret-op (make :I->M 'a))
         '[([:seq-re :<r a] (a)) ([:seq-re :<..r (a)] a)])))

(deftest selector-test
  ;; !! unverified
  (testing "Correct expression"
    (is (= (selector {})
           '()))
    (is (= (selector {'a :M})
           '((a))))
    (is (= (selector {'a :I})
           '[:- ((a) ([:seq-re :<..r nil])) (a ([:seq-re :<r nil]))]))
    (is (= (selector {'a :M 'b :N 'c :M})
           '((a) b (c))))
    (is (= (selector {'a :M, 'b :N, 'c :U})
           '[:- (a (b) c ([:seq-re :<..r nil])) (a (b) (c) ([:seq-re :<r nil]))])))

  (testing "Without simplification"
    (is (= (selector {} false)
           '[:- (([:seq-re :<..r nil])) (([:seq-re :<r nil]))]))
    (is (= (interpret-op (selector {'a :M} false))
           '[[(a ([:seq-re :<..r nil])) (a ([:seq-re :<r nil]))]]))))


(deftest equal-test
  (testing "Trivial cases"
    (are [x] (equal x x)
      nil [] [:- 'a 'b] [['a] :U] (seq-re :<r "x" "y"))
    (is (equal nil [[]] [[] []]))
    (is (equal [] [nil] [[[]]] [[[] []]] [:- [] []])))


  (testing "Non-equality with redundant variables"
    (is (not (equal '[[a] a] :N)))
    (is (not (equal '[[p] p] nil)))
    (is (not (equal '[:- b [[a] a]] 'b)))
    (is (not (equal (seq-re :<r '(a (a)) 'b '(c (c)))
                    (seq-re :<r nil 'b nil)))))


  (testing "Non-equality with different variable names"
    (is (equal '[:- a a] 'a))
    (is (not (equal '[:- a a] 'x))))


  (testing "Non-equality with missing variables"
    (is (not (equal '[[p] p] nil))))


  (testing "Non-equality with swapped variable names"
    (is (equal [['a] 'b] ['b ['a]]))
    (is (not (equal [['a] 'b] [['b] 'a]))))


  (testing "Equality of algebraic transformations"
    (is (equal '[:- a a] 'a))
    (is (not (equal '[:- a a] 'b)))
    (is (equal '[a a] '[a]))
    (is (equal '[[a]] 'a [:- 'a]))
    (is (not (equal '[["a"]] 'a)))
    (is (equal '[a] '[[[a]]]))
    (is (equal '[:- [[a] [b]] [a b]] '[[[a] b] [[b] a]]))
    (is (equal '[[[a] [b]] [a b]] '[:- [[a] b] [[b] a]]))
    (is (equal '[[p r] [q r]] '[:- [[p] [q]] r]))))



(deftest equiv-test
  (testing "Trivial cases"
    (are [x] (equiv x x)
      nil [] [:- 'a 'b] [['a] :U] (seq-re :<r "x" "y"))
    (is (equiv nil [[]] [[] []]))
    (is (equiv [] [nil] [[[]]] [[[] []]] [:- [] []])))


  (testing "Equivalence with redundant variables"
    (is (equiv '[[a] a] :N nil [[]] '(a (((a a))) a) [:- '(a nil (a :N))]))
    (is (equiv '[:- b [[a] a]] 'b))
    (is (equiv [:- "b" [["a"] "a"]] "b"))
    (is (equiv (seq-re :<r '(a (a)) 'b '(c (c))) (seq-re :<r nil 'b nil))))


  (testing "Equivalence with swapped variable names"
    (is (equiv [['a] 'b] [['b] 'a])))


  (testing "Equivalence despite missing variables"
    (is (equiv '[[p] p] nil))
    (is (not (equiv '[[p] q] nil))))


  (testing "Equivalence despite different variable names"
    (is (equiv '[:- a a] 'x))
    (is (equiv '["apple" "apple"] '[a]))
    (is (equiv '[[x]] 'a [:- "foo bar"]))
    (is (equiv '[boo] '[[[a]]]))
    (is (equiv '[:- [[a] [b]] [a b]] '[[[x] y] [[y] x]]))
    (is (equiv '[[["me"] ["you"]] ["me" "you"]] '[:- [[a] b] [[b] a]]))
    (is (equiv '[[p r] [q r]] '[:- [[x] [xx]] xxx])))


  (testing "Equivalence in algebraic demonstrations"
    ;; LoF, prim. Alg.
    ;; C1
    (is (equiv '((a))
               '[:- (((a)) (a)) ((a))]
               '((((a)) (a)) (((a)) a))
               '((((a)) a))
               '((((a)) a) ((a) a))
               '[:- ((((a))) ((a))) a]
               'a))
    ;; C2
    (is (equiv '[:- (a b) b]
               '[:- (((a)) b) b]
               '[:- (((a)) ((b))) b]
               '(((a) b) ((b) b))
               '(((a) b))
               '[:- (a) b]))
    ;; C3
    (is (equiv '[:- () a]
               '[:- (a) a]
               '(((a) a))
               '()))
    ;; C4
    (is (equiv '[:- ((a) b) a]
               '[:- ((a) b a) a]
               '[:- ((a b) b a) a]
               'a))
    ;; C5
    (is (equiv '[:- a a]
               '[:- ((a)) a]
               'a))
    ;; C6
    (is (equiv '[:- ((a) (b)) ((a) b)]
               '((((a) (b)) ((a) b)))
               '((((b)) (b)) (a))
               '((a))
               'a))
    ;; C7
    (is (equiv '(((a) b) c)
               '(((a) ((b))) c)
               '(((a c) ((b) c)))
               '[:- (a c) ((b) c)]))
    ;; C8
    (is (equiv '((a) (b r) (c r))
               '((a) (((b r) (c r))))
               '((a) (((b) (c)) r))
               '[:- ((a) (b) (c)) ((a) (r))]))
    ;; C9 (commented out because slow, but test passes)
    ;; (is (equiv '(((b) (r)) ((a) (r)) ((x) r) ((y) r))
    ;;            '(((b) (r)) ((a) (r)) ((x y) r))
    ;;            '[:- (b a ((x y) r)) (r ((x y) r))]
    ;;            '[:- (b a ((x y) r)) (r x y)]
    ;;            '[:- (b a ((x y) r) (r x y)) (r x y)]
    ;;            '[:- ((r) a b) (r x y)]))

    ;; uFORM iFORM (engl. Ed.)
    ;; p.7
    (is (equiv '((a) b)
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
    (is (equiv (seq-re :<r :M :M) (seq-re :<r :N :M) :N nil))
    (is (equiv (seq-re :<r :M :N) :M '()))
    (is (equiv (seq-re :<r nil nil) :U))
    ;; p.10
    (is (equiv [(seq-re :<r :M :M)] [(seq-re :<r :N :M)] [:N] '()))
    (is (equiv [(seq-re :<r :M :N)] [:M] '(()) nil))
    (is (equiv [(seq-re :<r nil nil)] [:U] :I))
    ;; p.11
    (is (equiv [:- (seq-re :<r nil nil) '()] [:- :U '()]
               [:- [(seq-re :<r nil nil)] '()] [:- :I '()]
               '()))
    (is (equiv (seq-re :<r [:- (seq-re :<r 'a 'b) '()] 'c)
               (seq-re :<r '() 'c)
               '((()) c)
               '(c)))
    (is (equiv (seq-re :<r 'a [:- (seq-re :<r 'b 'c) '()])
               (seq-re :<r 'a '())
               '(()) nil))
    ;; p.17
    (is (equiv (make :uncl "uncFo") (seq-re :<r "uncFo" "uncFo")))
    (is (equiv (seq-re :<r '() '()) '(()) nil))
    (is (equiv (seq-re :<r '(()) '(())) :U))))


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
    (let [dna [:N :N :N :N :N :N :N :N :N :N :N :N :N :N :N :N :U :U :U :U :N :U :N :U :U :U :U :U :N :U :N :U :I :I :I :I :I :I :I :I :N :N :I :I :N :N :I :I :M :M :M :M :I :M :I :M :U :U :M :M :N :U :I :M]]
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

