(ns formform.expr-test
  (:require [clojure.test :as t :refer [deftest is are testing]]
            [formform.calc :as calc]
            [formform.expr.symexpr :refer :all]
            [formform.expr.core :refer :all]
            [formform.expr.operators :refer :all]
            [formform.expr :refer [fns-with-specs]]
            [orchestra.spec.test :as stest]))

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
           (find-vars (make "a" 'a) {}) ;; should this be equal?
           )))

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

  (testing "Exotic variable names"
    ;; incorrect ordering due to symbols
    ; (is (= (find-vars '[ "apple tree" ("something" else) ])
    ;        '("apple tree" else "something")))
    ))


(defn- simplify-expr-chain-reversed [r->l? exprs env]
  (let [rev-exprs (fn [xs] (reverse (map #(if (arrangement? %)
                                            (cons (first %) (reverse (rest %)))
                                            %) xs)))]
    (rev-exprs (simplify-expr-chain {:rtl? r->l?} (rev-exprs exprs) {}))))

(def simplify-expr-chain->rtl (partial simplify-expr-chain-reversed true))
(def simplify-expr-chain->ltr (partial simplify-expr-chain-reversed false))

(comment
  ;; test
  (let [xc '[ a nil b ]]
    {:normal (simplify-expr-chain xc {})
     :rtl (simplify-expr-chain->rtl xc {})
     :reverse (reverse (simplify-expr-chain (reverse xc) {}))})
  
  (simplify-expr-chain '( nil ) {})
  )

;; ! add more tests with marks ()
(deftest simplify-expr-chain-test
  (testing "Correctness of reduction with one form per nesting-level"
    (are [x y] (= y (simplify-expr-chain x {}) (simplify-expr-chain->rtl x {}))
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
    (are [x y] (= y (simplify-expr-chain x {}) (simplify-expr-chain->rtl x {}))
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
    (are [x y] (= y (simplify-expr-chain x {}) (simplify-expr-chain->rtl x {}))
      '( [:- nil nil] ) '[nil]
      '( [:- () ()] )   '[()]
      '( [:- a a] )     '[a]

      '( [:- nil a] [:- nil ()] ) '[a] ;; (a (())) => (a)
      '( [:- a ()] [:- nil b] )   '[()] ;; (a () (b)) => (())
      ))

  (testing "Correctness of reduction via (de)generation rule across chain"
    (let [x [[:- 'a 'b] [:- 'a 'c]] ]
      (is (= '[[:- a b] c]
             (simplify-expr-chain x {}) (simplify-expr-chain->rtl x {}))))
    ;; (a b (a c)) => (a b (c))
    (let [x ['x 'a nil 'b 'x] ]
      (is (= '[x]
             (simplify-expr-chain x {}) (simplify-expr-chain->rtl x {}))))
    ;; (x (a ( (b (x))))) => (x (a b ())) => (x)
    (let [x [[:- 'a 'b] [:- 'a 'c] [:- 'c 'a 'd]] ]
      (is (= '[[:- a b] c d]
             (simplify-expr-chain x {})
             (simplify-expr-chain->rtl x {}))))
    ;; (a b (a c (c a d))) => (a b (c (d)))
    (let [x [[:- :U 'b] [:- 'a :I]] ]
      (is (= '[[:- :U b]]
             (simplify-expr-chain x {})
             (simplify-expr-chain->rtl x {}))))
    ;; (:U b (a :I)) => (:U b (a :U :I)) => (:U b (())) => (:U b)
    (let [x [[:- :U 'b] [:- 'a :I] 'c] ]
      (is (= '[[:- :U b]]
             (simplify-expr-chain x {})
             (simplify-expr-chain->rtl x {}))))
    ;; (:U b (a :I (c))) => (:U b (a () (c))) => (:U b (())) => (:U b)

    (let [x [:f* [:- :U 'a] [:- :U 'b]] ]
      (is (= '[:f* a [:- :U b]]
             (simplify-expr-chain {:rtl? true} x {})
             (simplify-expr-chain->ltr x {}))))
    ;; (((:f*) :U a) :U b) => (((:f*) a) :U b)
    (let [x [:f* :U :U] ]
      (is (= '[[:- :f* :U]]
             (simplify-expr-chain {:rtl? true} x {})
             (simplify-expr-chain->ltr x {}))))
    ;; (((:f*) :U) :U) => (((:f*)) :U) => (:f* :U)
    ))


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
    (is (= '[:seq-re :<r b [:U] a] (f (seq-re :<r 'b [:- :I 'a] 'a)))) )

  )


(deftest cnt>-test
  (testing "Simple content"
    (testing "FORM"
      (testing "equal"
        (are [x y] (= x y)
          (cnt> nil) nil
          (cnt> '()) '()))
      (testing "marked"
        (are [x y] (= x y)
          (cnt> '(nil))  '()
          (cnt> '(()))   nil
          (cnt> '(([]))) [])))

    (testing "Constant"
      (testing "equal"
        (are [x y] (= x y)
          (cnt> :N) nil
          (cnt> :M) '()
          (cnt> :U) :U
          (cnt> :I) '(:U)))
      (testing "marked"
        (are [x y] (= x y)
          (cnt> '(:N)) '()
          (cnt> '(:M)) nil
          (cnt> '(:U)) '(:U)
          (cnt> '(:I)) :U)))

    (testing "variable"
      (testing "without env"
        (is (= (cnt> 'a) 'a))
        (is (= (cnt> "a") "a")))

      (testing "with env"
        (is (= (cnt> 'a {'a :M})
               (cnt> '(a) {'a :N})
               (cnt> '((a (b))) {'a :I 'b :I})
               '()))
        (is (= (cnt> "a" {"a" :U}) :U)))))

  (testing "Nested content"
    (testing "reduced by calling"
      (is (= (cnt> '(nil () nil () nil))
             (cnt> '(() :M (:N)))
             nil))
      (is (= (cnt> '(:U nil :U :U))
             '(:U)))
      (is (= (cnt> '(:I nil :I (:U)))
             :U))
      (is (= (cnt> '("foo" "foo" "bar" "foo"))
             '("foo" "bar")))
      (is (= (cnt> '(a b b a b a))
             '(a b)))
      (is (= (cnt> '((a) (a) (a)))
             'a))
      (is (= (cnt> '((a (b)) (a (b))))
             '((a (b))))))

    (testing "reduced by crossing"
      (is (= (cnt> '(()))
             (cnt> '((nil)))
             (cnt> '(((:M))))
             (cnt> '((((nil) nil) nil) nil))
             nil))
      (is (= (cnt> '((())))
             (cnt> '(((nil))))
             (cnt> '(((((:N))))))
             (cnt> '(((((nil) nil) nil) nil) nil))
             '()))
      (is (= (cnt> '(((()) (()))))
             (cnt> '((((nil)) (:M))))
             (cnt> '(((:U)) ((:I))))
             nil))
      (is (= (cnt> '(((a))))
             '(a)))
      (is (= (cnt> '((a)))
             'a))
      (is (= (cnt> '((x)) {'x :U})
             :U))
      (is (= (cnt> '(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((x))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) {'x :U})
             '(:U))))

    (testing "irreducable"
      (is (= (cnt> '(a b))
             '(a b)))
      ;; should these be reducable? pattern-matching?
      ;; -> might get too complex for eval
      (is (= (cnt> '((a (b)) ((b) a)))
             '((a (b)) ((b) a)))))))


(deftest ctx>-test
  (testing "Relations of simple expressions"
    (testing "FORMs"
      (is (= (ctx> [ nil nil ]) [ ]))
      (is (= (ctx> [ '() '() ]) [ '() ])))

    (testing "Constants"
      (is (= (ctx> [ :N :N ]) [ ]))
      (is (= (ctx> [ :M :M ]) [ '() ]))
      (is (= (ctx> [ :U :U ]) [ :U ]))
      (is (= (ctx> [ [:U] [:U] ]) [ '(:U) ]))
      (is (= (ctx> [ :I :I ]) [ '(:U) ]))
      (is (= (ctx> [ [:I] [:I] ]) [ :U ]))
      (is (= (ctx> [ :M :N ]) [ '() ]))
      (is (= (ctx> [ :U :N ]) [ :U ]))
      (is (= (ctx> [ :I :N ]) [ '(:U) ]))
      (is (= (ctx> [ :U :I ]) [ '() ]))
      (is (= (ctx> [ [:U] :I ]) [ '(:U) ]))
      (is (= (ctx> [ [:I] :U ]) [ :U ]))
      (is (= (ctx> [ [:U] [:I] ]) [ '() ]))
      (is (= (ctx> [ :U :M ]) [ '() ]))
      (is (= (ctx> [ :I :M ]) [ '() ])))

    (testing "Variables"
      (testing "without env"
        (is (= (ctx> [ 'a 'b ])
               [ 'a 'b ]))
        (is (= (ctx> [ 'a 'a ])
               [ 'a ]))
        (is (= (ctx> [ 'a "a" ]) ;; should be equal?
               [ 'a "a" ])))

      (testing "with env"
        (is (= (ctx> [ 'a 'a ] {'a :U})
               [ :U ]))
        (is (= (ctx> [ 'a 'b ] {'a :U 'b :I})
               [ '() ]))
        (is (= (ctx> [ 'a "a" ] {'a :U "a" :I}) ;; should be equal?
               [ '() ]))
        (is (= (ctx> [ 'a 'b ] {'a :U})
               [ :U 'b ]))
        (is (= (ctx> [ 'x ] {'x 'y})
               [ 'y ])))

      (testing "with recursive env"
        ;; ? infinite recursion or dissoc from env on first interpretation
        (is (= (ctx> [ 'x ] {'x 'x})
               (ctx> [ 'x 'x ] {'x 'x})
               '[x])
            (= (ctx> [ 'x ] {'x [:- 'x]})
               '[x]))))

    (testing "mixed"
      (is (= (ctx> [ '(()) nil :N '(((()))) '(() () ()) ])
             [ ]))
      (is (= (ctx> [ '((() ())) :M '() ])
             [ '() ]))
      (is (= (ctx> [ :U '((:U)) :U '((:U) (:U)) ])
             [ :U ]))
      (is (= (ctx> [ '(:U) '(((:U))) :I '((:I) (:I)) ])
             [ '(:U) ]))
      (is (= (ctx> [ :U '(:I) ])
             [ :U ]))
      (is (= (ctx> [ '(:I) '(:U) ])
             [ '() ]))
      (is (= (ctx> [ '((:U)) '(:U) ])
             [ '() ]))
      (is (= (ctx> [ :U '(:I) ])
             [ :U ]))
      (is (= (ctx> [ '(:U) '((:U)) ])
             [ '() ]))
      (is (= (ctx> [ '((:I)) :U ])
             [ '() ]))))

  (testing "Relations of complex expressions"
    (testing "pure FORMs"
      (is (= (ctx> [ '(()) ])
             [ ]))
      (is (= (ctx> [ '(() ()) ])
             [ ]))
      (is (= (ctx> [ '() '(() ()) ])
             [ '() ]))
      (is (= (ctx> [ '(((() (() ()))) ((() (())) (() ()) (()))) ])
             [ ])))

    (testing "nested variables (reflexion rule)"
      (is (= (ctx> '[ ((a)) ])
             [ 'a ]))
      (is (= (ctx> '[ ((a b c)) ])
             [ 'a 'b 'c ]))
      (is (= (ctx> '[ ((a ((b c)) d)) ((e ((f)))) ])
             [ 'a 'b 'c 'd 'e 'f ]))
      (is (= (ctx> '[ (a ((b c)) d) ((e (f))) ])
             [ '(a b c d) 'e '(f) ]))
      (is (= (ctx> [ '([:- a]) '[:- a] ])
             [ '() ]))
      (is (= (ctx> [ '([:- a]) '[:- b] ])
             [ '(a) 'b ])))

    (testing "nested U/I relations (generation rule)"
      (is (= (ctx> '[ :U (:I) ])
             [ :U ]))
      (is (= (ctx> '[ :U (:N (:I)) ])
             [ '() ]))
      (is (= (ctx> '[ :U (:N (:N (:I))) ])
             [ :U ]))
      (is (= (ctx> '[ :U (:N (:U (:N (:I)))) ])
             [ '() ]))
      (is (= (ctx> '[ :N (:U (:N (:N (:I)))) ])
             [ '(:U) ]))
      (is (= (ctx> '[ :I (:U) ])
             [ '(:U) ]))
      (is (= (ctx> '[ :I (:N (:U)) ])
             [ '() ]))
      (is (= (ctx> '[ :I (:N (:N (:U))) ])
             [ '(:U) ]))
      (is (= (ctx> '[ :I (:N (:I (:N (:U)))) ])
             [ '() ]))
      (is (= (ctx> '[ :N (:I (:N (:N (:U)))) ])
             [ :U ]))
      (is (= (ctx> '[ :U (:I a) ])
             [ :U ]))
      (is (= (ctx> '[ :U ((a :U) :I) ])
             [ :U ]))
      (is (= (ctx> '[ :U ((a :I) :U) ])
             [ '() ]))
      (is (= (ctx> '[ :I ((a (b :U (c))) c) ])
             [ '(:U) '((a) c) ]))))

  (testing "Degeneration in nested expressions"
    (is (= (ctx> '[ a b (a c (c a d)) ] {})
           '[ a b (c (d)) ]))
    (is (= (ctx> '[ (a (b a c) (a c (b c d a))) ] {})
           '[ (a (b c) (c (b d))) ]))
    (is (= (ctx> '[ x (a (b x)) (y (a x (b x))) ])
           '[ x (a (b)) (y) ])))

  (testing "Long relations" ;; ? remove due to triviality
    (is (= (ctx> (repeat 100 nil)) [ ]))
    (is (= (ctx> (repeat 100 '())) [ '() ]))
    (is (= (ctx> (repeat 100 :U)) [ :U ]))
    (is (= (ctx> (repeat 100 '(:U))) [ '(:U) ]))
    (is (= (ctx> (repeat 100 :N)) [ ]))
    (is (= (ctx> (repeat 100 :M)) [ '() ]))
    (is (= (ctx> (repeat 100 :U)) [ :U ]))
    (is (= (ctx> (repeat 100 :I)) [ '(:U) ])))

  (testing "Operators in context"
    (is (= '[[:seq-re :<r a b]]
           (ctx> [[:- [:seq-re :<r 'a 'b]]])))
    ;; law of crossing cannot be applied to (known) operators
    (is (= '[(a b c)]
           (ctx> [ [[:- 'a 'b 'c]] ])))
    (is (= '[:? y] ;; operator unknown
           (ctx> [ [[:? 'y]] ])))
    (is (= '[([:fdna ["foo"] (:N :U :U :U)])]
           (ctx> [ [[:uncl "foo"]] ])))
    (is (= '[([:seq-re :<..r a b])]
           (ctx> [ [[:seq-re :<..r 'a 'b]] ])))
    (is (= '[(z)]
           (ctx> [ [[:mem [['x 'y]] 'z]] ])))
    ;; law of calling can be applied to (all?) operators
    ;; (although there are practical limitations in simplification algo)
    (is (= '[a b c]
           (ctx> [[:- 'a 'b 'c] [:- 'a 'b 'c]])))
    (is (= '[z]
           (ctx> [[:mem [['x 'y]] 'z] [:mem [['x 'y]] 'z]])
           (ctx> [[:mem [['x 'y] ['y 'z]] 'x] [:mem [['x 'y] ['y 'z]] 'x]])))
    (is (= '[:U]
           (ctx> [[:mem [['x :U]] 'x] [:mem [['x :U]] 'x]])))
    (is (= '[[:fdna ["foo"] (:N :U :U :U)]]
           (ctx> [[:uncl "foo"] [:uncl "foo"] [:uncl "foo"]])
           (ctx> [[:fdna ["foo"] [:N :U :U :U]]
                  [:fdna ["foo"] [:N :U :U :U]]])))
    (is (= '[[:seq-re :<..r a b]]
           (ctx> [[:seq-re :<..r 'a 'b] [:seq-re :<..r 'a 'b]])))
    ;; cannot cross re-entry boundary in (de)generation!
    (is (= '[[:seq-re :<r nil a] a]
           (ctx> [[:seq-re :<r 'a 'a] 'a])
           (ctx> [[:- [:seq-re :<r 'a 'a] 'a]])))
    (is (= '[[:seq-re :<r [:U] a] :U]
           (ctx> [[:seq-re :<r :I 'a] :U])
           (ctx> [[:seq-re :<r [:- :I] 'a] :U])))
    (is (= '[[:seq-re :<r a :U] [:U]]
           (ctx> [[:seq-re :<r 'a :U] :I])
           (ctx> [[:seq-re :<r 'a [:- :U]] :I])))
    ;; dominance of mark still holds
    (is (= '[[]]
           (ctx> [[:seq-re :<r 'a 'b] []])
           (ctx> [[] [:seq-re :<r 'a 'b]])
           (ctx> [[] [[[:seq-re :<r 'a 'b]]]])
           (ctx> [:U [:seq-re :<r 'a 'b] :I])))
    (is (= '[[]]
           (ctx> [[:seq-re :<r nil nil] :I])
           (ctx> [[:seq-re :<r nil] :U])
           (ctx> [[:seq-re :<r nil nil] [:seq-re :<r nil]])))
    (is (= '[[]]
           (ctx> [[:- [:seq-re :<r 'a 'b] []]])
           (ctx> [[:fdna [] [:M]] [:seq-re :<r 'a 'b]])
           (ctx> [[:fdna ["foo"] [:N :U :U :U]] [:fdna [] [:M]] ]))))
  )


(deftest =>-test
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
    (apply str varorder "::" (calc/dna->digits dna calc/nmui-code))))

(deftest =>*-test
  (testing "Correctness of returned combinatorial space"
    (is (= [:fdna '() [:N] ]
           (=>* (make nil))))

    (is (= [:fdna '(a) [:M :I :U :N] ]
           (=>* (make 'a))))

    (is (= [:fdna '(a b) [:M :M :M :M
                          :M :I :M :I
                          :M :M :U :U
                          :M :I :U :N] ]
           (=>* (make 'a 'b))))

    (is (= [:fdna '(a b c)
            [:M :M :M :M  :M :M :M :M  :M :M :M :M  :M :M :M :M
             :M :M :M :M  :M :I :M :I  :M :M :M :M  :M :I :M :I
             :M :M :M :M  :M :M :M :M  :M :M :U :U  :M :M :U :U
             :M :M :M :M  :M :I :M :I  :M :M :U :U  :M :I :U :N] ]
           (=>* (make 'a 'b 'c)))))

  (testing "Correctness of evaluation for simple seq-re FORMs"
    (is (= '[:fdna [a] [:N :I :I :I]]
           (=>* (seq-re :<r 'a)) (=>* (seq-re :<..r. 'a))))
    (is (= '[:fdna [a] [:N :U :U :U]]
           (=>* (seq-re :<..r 'a))))

    (is (= '[:fdna [a] [:M :U :U :I]]
           (=>* (seq-re :<r_ 'a)) (=>* (seq-re :<..r._ 'a))))
    (is (= '[:fdna [a] [:M :I :I :U]]
           (=>* (seq-re :<..r_ 'a))))

    (is (= '[:fdna [a] [:N :I :I :I]]
           (=>* (seq-re :<r' 'a)) (=>* (seq-re :<..r'. 'a))))
    (is (= '[:fdna [a] [:N :U :U :U]]
           (=>* (seq-re :<..r' 'a))))

    (is (= '[:fdna [a] [:M :I :M :I]]
           (=>* (seq-re :<r'_ 'a)) (=>* (seq-re :<..r'._ 'a))))
    (is (= '[:fdna [a] [:M :M :U :U]]
           (=>* (seq-re :<..r'_ 'a)))))

  (testing "Correctness of evaluation in complex seq-re FORMs"
    (is (= '[:fdna [a b] [:N :N :N :N
                          :N :I :N :I
                          :I :I :I :I
                          :I :U :I :U]]
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
           "[l e r]::3123121221213213311310321331321001231032230132103113121211113210"))

    ))

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
           (splice-ctx
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
              [[a [[[[[:f* [[:f*]]] [[:f*] [[[:f*]]]]] [:f*]]]]]]]]))
    )
  (testing "--defocus flags"
    (is (= (interpret-walk* {:--defocus #{:ops}} [:- :U [:- 'a [:I]]])
           '[:- [:seq-re :<r nil nil] [:- a [[[:seq-re :<r nil nil]]]]]))
    (is (= (interpret-walk* {:--defocus #{:mem}} [:- :U [:- 'a [:I]]])
           '[[[:mem [[:f* [[:f*]]]] :f*] 
              [[a [[[:mem [[:f* [[:f*]]]] :f*]]]]]]]))
    )
  )


(deftest simplify-op-test
  (testing "formDNA"
    (testing "Basic functionality"
      (is (= (simplify-op (make :fdna ['a] [:N :U :I :M]) {'a :U}) :I))
      (is (= (simplify-op (make :fdna ['a 'b] [:N :U :I :M
                                               :U :I :M :I
                                               :I :M :I :U
                                               :M :I :U :N]) {'a :M})
             '[:fdna [b] [:N :U :I :M]])))

    (testing "Partial matches in dictionary"
      (is (= (simplify-op (make :fdna ['a] [:N :U :I :M]) {'x :M})
             '[:fdna [a] [:N :U :I :M]]))
      (is (= (simplify-op (make :fdna ['a] [:N :U :I :M]) {'x :M 'a :U})
             :I)))

    (testing "Correctness of transformations"
      (are [x y] (= (simplify-op (make :fdna ['a] [:N :U :I :M]) {'a x}) y)
        :N :M
        :U :I
        :I :U
        :M :N)

      (is (= (simplify-op
              (make :fdna ['a 'b] [:N :U :I :M
                                   :U :I :M :I
                                   :I :M :I :U
                                   :M :I :U :N]) {'a :U})
             '[:fdna [b] [:I :M :I :U]]))))

  (testing "unclear FORMs"
    (testing "Basic functionality"
      (is (= (simplify-op (make :uncl "hey") {})
             '[:fdna ["hey"] [:N :U :U :U]]))

      (are [x y] (= (simplify-op (make :uncl "unkFo") {"unkFo" x}) y)
        :N :U
        :U :U
        :I :U
        :M :N)

      ;; !! this is correct, but what happened exactly?
      (is (= (simplify (make :uncl "hey") {"hey" :M})
             nil))))

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

    ;; ? move to ctx>-test
    (testing "In expression context"
      (is (= (ctx> [(make :mem [['a :U]] ['a] 'b)])
             (ctx> [(make :mem [['a :U]] ['a]) 'b])
             '[(:U) b]))
      (is (= (ctx> [(make :mem [['a :U]] 'a) 'b])
             '[:U b]))
      ;; env substitution has priority over degeneration from observed values
      (is (= (ctx> ['y (make :mem [['y 'z]] 'y)])
             '[y z]))
      ;; rems can shadow previous rems or given reduction env
      (is (= (ctx> ['x (make :mem [['y 'z]] 'x)] {'x 'y})
             (ctx> ['x (make :mem [['x 'z]] 'x)] {'x 'y})
             '[y z]))
      (is (= (ctx> [(make :mem [['y 'x]] 'x)] {'x 'y})
             (ctx> [(make :mem [['y [:- 'x]]] 'x)] {'x 'y})
             '[y]))
      (is (= (ctx> [(make :mem [['x 'y] ['y 'z]] 'w)] {'w 'x})
             (ctx> [(make :mem [['y 'z] ['x 'y]] 'w)] {'w 'x})
             '[z]))
      (is (= (ctx> [(make :mem [['x [:- 'x 'z]]] 'x)] {'x 'y})
             '[y z]))
      (is (= (ctx> ['x (make :mem [['y 'x]] 'x)] {'x 'y})
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
               [[:M] [[:N->M a]]]
               [[:I] [[:U->M a]]]
               [[:U] [[:I->M a]]]
               [[:N] [[:M->M a]]]]))
      (is (= (interpret-op (make :fdna ['a 'b] [:M :I :N :I
                                                :U :M :I :N
                                                :U :M :N :I
                                                :M :M :I :M]))
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
               '[:mem [[:f* ((((((:f*) x))) x))] [:f1 ((:f*) x)]] :f1])))
      )))

(deftest filter-rems-test
  (testing "Removal of unreferenced shadowed rems"
    (is (= (filter-rems '[[a (x)] [x a] [x :M]] ['x])
           '[[x :M]]))
    (is (= (filter-rems '[[a :N] [x :M] [a (x)]] ['a])
           '[[a (x)] [x :M]]))))


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

(deftest sel-test
  ;; !! unverified
  (testing "Correct expression"
    (is (= (sel {})
           '()))
    (is (= (sel {'a :M})
           '((a))))
    (is (= (sel {'a :I})
           '[:- ((a) ([:seq-re :<..r nil])) (a ([:seq-re :<r nil]))]))
    (is (= (sel {'a :M 'b :N 'c :M})
           '((a) b (c))))
    (is (= (sel {'a :M, 'b :N, 'c :U})
           '[:- (a (b) c ([:seq-re :<..r nil])) (a (b) (c) ([:seq-re :<r nil]))])))

  (testing "Without simplification"
    (is (= (sel {} false)
           '[:- (([:seq-re :<..r nil])) (([:seq-re :<r nil]))]))
    (is (= (interpret-op (sel {'a :M} false))
           '[[(a ([:seq-re :<..r nil])) (a ([:seq-re :<r nil]))]])))) 


(deftest equal-test
  (testing "Trivial cases"
    (are [x] (equal x x)
      nil [] [:- 'a 'b] [['a] :U] (seq-re :<r "x" "y"))
    (is (equal nil [[]] [[] []]))
    (is (equal [] [nil] [[[]]] [[[] []]] [:- [] []]))
    )

  (testing "Non-equality with redundant variables"
    (is (not (equal '[[a] a] :N)))
    (is (not (equal '[[p] p] nil)))
    (is (not (equal '[:- b [[a] a]] 'b)))
    (is (not (equal (seq-re :<r '(a (a)) 'b '(c (c)))
                    (seq-re :<r nil 'b nil))))
    )

  (testing "Non-equality with different variable names"
    (is (equal '[:- a a] 'a))
    (is (not (equal '[:- a a] 'x)))
    )

  (testing "Non-equality with missing variables"
    (is (not (equal '[[p] p] nil)))
    )

  (testing "Non-equality with swapped variable names"
    (is (equal [['a] 'b] ['b ['a]]))
    (is (not (equal [['a] 'b] [['b] 'a])))
    )

  (testing "Equality of algebraic transformations"
    (is (equal '[:- a a] 'a))
    (is (not (equal '[:- a a] 'b)))
    (is (equal '[a a] '[a]))
    (is (equal '[[a]] 'a [:- 'a]))
    (is (not (equal '[["a"]] 'a)))
    (is (equal '[a] '[[[a]]]))
    (is (equal '[:- [[a] [b]] [a b]] '[[[a] b] [[b] a]]))
    (is (equal '[[[a] [b]] [a b]] '[:- [[a] b] [[b] a]]))
    (is (equal '[[p r] [q r]] '[:- [[p] [q]] r]))
    )
  )

(deftest equiv-test
  (testing "Trivial cases"
    (are [x] (equiv x x)
      nil [] [:- 'a 'b] [['a] :U] (seq-re :<r "x" "y"))
    (is (equiv nil [[]] [[] []]))
    (is (equiv [] [nil] [[[]]] [[[] []]] [:- [] []]))
    )

  (testing "Equivalence with redundant variables"
    (is (equiv '[[a] a] :N nil [[]] '(a (((a a))) a) [:- '(a nil (a :N))]))
    (is (equiv '[:- b [[a] a]] 'b))
    (is (equiv [:- "b" [["a"] "a"]] "b"))
    (is (equiv (seq-re :<r '(a (a)) 'b '(c (c))) (seq-re :<r nil 'b nil)))
    )

  (testing "Equivalence with swapped variable names"
    (is (equiv [['a] 'b] [['b] 'a]))
    )

  (testing "Equivalence despite missing variables"
    (is (equiv '[[p] p] nil))
    (is (not (equiv '[[p] q] nil)))
    )

  (testing "Equivalence despite different variable names"
    (is (equiv '[:- a a] 'x))
    (is (equiv '["apple" "apple"] '[a]))
    (is (equiv '[[x]] 'a [:- "foo bar"]))
    (is (equiv '[boo] '[[[a]]]))
    (is (equiv '[:- [[a] [b]] [a b]] '[[[x] y] [[y] x]]))
    (is (equiv '[[["me"] ["you"]] ["me" "you"]] '[:- [[a] b] [[b] a]]))
    (is (equiv '[[p r] [q r]] '[:- [[x] [xx]] xxx]))
    )

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
    ; (is (equiv '(((b) (r)) ((a) (r)) ((x) r) ((y) r))
    ;            '(((b) (r)) ((a) (r)) ((x y) r))
    ;            '[:- (b a ((x y) r)) (r ((x y) r))]
    ;            '[:- (b a ((x y) r)) (r x y)]
    ;            '[:- (b a ((x y) r) (r x y)) (r x y)]
    ;            '[:- ((r) a b) (r x y)]))

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
    (is (equiv (seq-re :<r '(()) '(())) :U))
    )

  ) 


(comment

  
  

  
  
  )
